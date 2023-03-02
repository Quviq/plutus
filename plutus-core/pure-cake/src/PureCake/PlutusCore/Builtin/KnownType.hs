-- editorconfig-checker-disable-file
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE StrictData            #-}

module PureCake.PlutusCore.Builtin.KnownType
    ( KnownTypeError
    , throwKnownTypeErrorWithCause
    , KnownBuiltinTypeIn
    , MakeKnownM (..)
    , ReadKnownM
    , readKnownConstant
    , MakeKnownIn (..)
    , MakeKnown
    , ReadKnownIn (..)
    , ReadKnown
    ) where

import PlutusPrelude

import PureCake.PlutusCore.Builtin.Emitter
import PureCake.PlutusCore.Builtin.HasConstant
import PureCake.PlutusCore.Builtin.Polymorphism
import PureCake.PlutusCore.Core
import PureCake.PlutusCore.Evaluation.Machine.Exception
import PureCake.PlutusCore.Evaluation.Result

import Control.Monad.Except
import Data.DList (DList)
import Data.Text (Text)
import GHC.Exts (inline, oneShot)
import GHC.TypeLits
import Universe

-- | A constraint for \"@a@ is a 'ReadKnownIn' and 'MakeKnownIn' by means of being included
-- in @uni@\".
type KnownBuiltinTypeIn uni val a =
    (HasConstantIn uni val, GEq uni, uni `Contains` a)

-- | A constraint for \"@a@ is a 'ReadKnownIn' and 'MakeKnownIn' by means of being included
-- in @UniOf term@\".
type KnownBuiltinType val a = KnownBuiltinTypeIn (UniOf val) val a

-- | Attach a @cause@ to a 'KnownTypeError' and throw that.
-- Note that an evaluator might require the cause to be computed lazily for best performance on the
-- happy path, hence this function must not force its first argument.
-- TODO: wrap @cause@ in 'Lazy' once we have it.
throwKnownTypeErrorWithCause
    :: (MonadError (ErrorWithCause err cause) m, AsUnliftingError err, AsEvaluationFailure err)
    => cause -> KnownTypeError -> m void
throwKnownTypeErrorWithCause cause = \case
    KnownTypeUnliftingError unlErr -> throwingWithCause _UnliftingError unlErr $ Just cause
    KnownTypeEvaluationFailure     -> throwingWithCause _EvaluationFailure () $ Just cause

-- | The monad that 'makeKnown' runs in.
-- Equivalent to @ExceptT KnownTypeError Emitter@, except optimized in two ways:
--
-- 1. everything is strict
-- 2. has the 'MakeKnownSuccess' constructor that is used for returning a value with no logs
--    attached, which is the most common case for us, so it helps a lot not to construct and
--    deconstruct a redundant tuple
--
-- Moving from @ExceptT KnownTypeError Emitter@ to this data type gave us a speedup of 8% of total
-- evaluation time.
--
-- Logs are represented as a 'DList', because we don't particularly care about the efficiency of
-- logging, since there's no logging on the chain and builtins don't emit much anyway. Otherwise
-- we'd have to use @text-builder@ or @text-builder-linear@ or something of this sort.
data MakeKnownM a
    = MakeKnownFailure (DList Text) KnownTypeError
    | MakeKnownSuccess a
    | MakeKnownSuccessWithLogs (DList Text) a

-- | Prepend logs to a 'MakeKnownM' computation.
withLogs :: DList Text -> MakeKnownM a -> MakeKnownM a
withLogs logs1 = \case
    MakeKnownFailure logs2 err       -> MakeKnownFailure (logs1 <> logs2) err
    MakeKnownSuccess x               -> MakeKnownSuccessWithLogs logs1 x
    MakeKnownSuccessWithLogs logs2 x -> MakeKnownSuccessWithLogs (logs1 <> logs2) x
{-# INLINE withLogs #-}

instance Functor MakeKnownM where
    fmap _ (MakeKnownFailure logs err)       = MakeKnownFailure logs err
    fmap f (MakeKnownSuccess x)              = MakeKnownSuccess (f x)
    fmap f (MakeKnownSuccessWithLogs logs x) = MakeKnownSuccessWithLogs logs (f x)
    {-# INLINE fmap #-}

    -- Written out explicitly just in case (see @fmap@ above for what the case might be).
    _ <$ MakeKnownFailure logs err       = MakeKnownFailure logs err
    x <$ MakeKnownSuccess _              = MakeKnownSuccess x
    x <$ MakeKnownSuccessWithLogs logs _ = MakeKnownSuccessWithLogs logs x
    {-# INLINE (<$) #-}

instance Applicative MakeKnownM where
    pure = MakeKnownSuccess
    {-# INLINE pure #-}

    MakeKnownFailure logs err       <*> _ = MakeKnownFailure logs err
    MakeKnownSuccess f              <*> a = fmap f a
    MakeKnownSuccessWithLogs logs f <*> a = withLogs logs $ fmap f a
    {-# INLINE (<*>) #-}

    -- Better than the default implementation, because the value in the 'MakeKnownSuccess' case
    -- doesn't need to be retained.
    MakeKnownFailure logs err       *> _ = MakeKnownFailure logs err
    MakeKnownSuccess _              *> a = a
    MakeKnownSuccessWithLogs logs _ *> a = withLogs logs a
    {-# INLINE (*>) #-}

instance Monad MakeKnownM where
    MakeKnownFailure logs err       >>= _ = MakeKnownFailure logs err
    MakeKnownSuccess x              >>= f = f x
    MakeKnownSuccessWithLogs logs x >>= f = withLogs logs $ f x
    {-# INLINE (>>=) #-}

    (>>) = (*>)
    {-# INLINE (>>) #-}

-- Normally it's a good idea for an exported abstraction not to be a type synonym, since a @newtype@
-- is cheap, looks good in error messages and clearly emphasize an abstraction barrier. However we
-- make 'ReadKnownM' type synonyms for convenience: that way we don't need to derive all the
-- instances (and add new ones whenever we need them), wrap and unwrap all the time
-- (including in user code), which can be non-trivial for such performance-sensitive code (see e.g.
-- 'coerceVia' and 'coerceArg') and there is no abstraction barrier anyway.
-- | The monad that 'readKnown' runs in.
type ReadKnownM = Either KnownTypeError

-- See Note [Unlifting values of built-in types].
-- | Convert a constant embedded into a PLC term to the corresponding Haskell value.
readKnownConstant :: forall val a. KnownBuiltinType val a => val -> ReadKnownM a
-- Note [Performance of ReadKnownIn and MakeKnownIn instances]
readKnownConstant val = asConstant val >>= oneShot \case
    Some (ValueOf uniAct x) -> do
        let uniExp = knownUni @_ @(UniOf val) @a
        -- 'geq' matches on its first argument first, so we make the type tag that will be known
        -- statically (because this function will be inlined) go first in order for GHC to
        -- optimize some of the matching away.
        case uniExp `geq` uniAct of
            Just Refl -> pure x
            Nothing   -> Left . KnownTypeUnliftingError $ coerce ("" :: Text)
{-# INLINE readKnownConstant #-}

-- See Note [Performance of ReadKnownIn and MakeKnownIn instances].
class uni ~ UniOf val => MakeKnownIn uni val a where
    -- See Note [Cause of failure].
    -- | Convert a Haskell value to the corresponding PLC val.
    -- The inverse of 'readKnown'.
    makeKnown :: a -> MakeKnownM val
    default makeKnown :: KnownBuiltinType val a => a -> MakeKnownM val
    -- Everything on evaluation path has to be strict in production, so in theory we don't need to
    -- force anything here. In practice however all kinds of weird things happen in tests and @val@
    -- can be non-strict enough to cause trouble here, so we're forcing the argument. Looking at the
    -- generated Core, the forcing amounts to pulling a @case@ out of the 'fromConstant' call,
    -- which doesn't affect the overall cost and benchmarking results suggest the same.
    --
    -- Note that the value is only forced to WHNF, so care must be taken to ensure that every value
    -- of a type from the universe gets forced to NF whenever it's forced to WHNF.
    makeKnown x = pure . fromValue $! x
    {-# INLINE makeKnown #-}

type MakeKnown val = MakeKnownIn (UniOf val) val

-- See Note [Performance of ReadKnownIn and MakeKnownIn instances].
class uni ~ UniOf val => ReadKnownIn uni val a where
    -- See Note [Cause of failure].
    -- | Convert a PLC val to the corresponding Haskell value.
    -- The inverse of 'makeKnown'.
    readKnown :: val -> ReadKnownM a
    default readKnown :: KnownBuiltinType val a => val -> ReadKnownM a
    -- If 'inline' is not used, proper inlining does not happen for whatever reason.
    readKnown = inline readKnownConstant
    {-# INLINE readKnown #-}

type ReadKnown val = ReadKnownIn (UniOf val) val

instance MakeKnownIn uni val a => MakeKnownIn uni val (EvaluationResult a) where
    makeKnown EvaluationFailure     = MakeKnownFailure mempty KnownTypeEvaluationFailure
    makeKnown (EvaluationSuccess x) = makeKnown x
    {-# INLINE makeKnown #-}

-- Catching 'EvaluationFailure' here would allow *not* to short-circuit when 'readKnown' fails
-- to read a Haskell value of type @a@. Instead, in the denotation of the builtin function
-- the programmer would be given an explicit 'EvaluationResult' value to handle, which means
-- that when this value is 'EvaluationFailure', a PLC 'Error' was caught.
-- I.e. it would essentially allow us to catch errors and handle them in a programmable way.
-- We forbid this, because it complicates code and isn't supported by evaluation engines anyway.
instance
        ( TypeError ('Text "‘EvaluationResult’ cannot appear in the type of an argument")
        , uni ~ UniOf val
        ) => ReadKnownIn uni val (EvaluationResult a) where
    readKnown _ = throwing _UnliftingError "Panic: 'TypeError' was bypassed"
    -- Just for 'readKnown' not to appear in the generated Core.
    {-# INLINE readKnown #-}

instance MakeKnownIn uni val a => MakeKnownIn uni val (Emitter a) where
    makeKnown a = case runEmitter a of
        (x, logs) -> withLogs logs $ makeKnown x
    {-# INLINE makeKnown #-}

instance
        ( TypeError ('Text "‘Emitter’ cannot appear in the type of an argument")
        , uni ~ UniOf val
        ) => ReadKnownIn uni val (Emitter a) where
    readKnown _ = throwing _UnliftingError "Panic: 'TypeError' was bypassed"
    -- Just for 'readKnown' not to appear in the generated Core.
    {-# INLINE readKnown #-}

instance HasConstantIn uni val => MakeKnownIn uni val (SomeConstant uni rep) where
    makeKnown = coerceArg $ pure . fromConstant
    {-# INLINE makeKnown #-}

instance HasConstantIn uni val => ReadKnownIn uni val (SomeConstant uni rep) where
    readKnown = coerceVia (fmap SomeConstant .) asConstant
    {-# INLINE readKnown #-}

instance uni ~ UniOf val => MakeKnownIn uni val (Opaque val rep) where
    makeKnown = coerceArg pure
    {-# INLINE makeKnown #-}

instance uni ~ UniOf val => ReadKnownIn uni val (Opaque val rep) where
    readKnown = coerceArg pure
    {-# INLINE readKnown #-}
