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
    ( MakeKnownM (..)
    , MakeKnownIn (..)
    ) where

import PureCake.PlutusCore.Builtin.HasConstant
import PureCake.PlutusCore.Core

import Data.DList (DList)
import Data.Text (Text)
import Universe

-- | A constraint for \"@a@ is a 'ReadKnownIn' and 'MakeKnownIn' by means of being included
-- in @uni@\".
type KnownBuiltinTypeIn uni val a =
    (HasConstantIn uni val, GEq uni, uni `Contains` a)

-- | A constraint for \"@a@ is a 'ReadKnownIn' and 'MakeKnownIn' by means of being included
-- in @UniOf term@\".
type KnownBuiltinType val a = KnownBuiltinTypeIn (UniOf val) val a

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
