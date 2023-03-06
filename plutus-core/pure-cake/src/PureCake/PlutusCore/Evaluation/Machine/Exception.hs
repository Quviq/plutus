-- editorconfig-checker-disable-file
-- | The exceptions that an abstract machine can throw.

-- appears in the generated instances
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module PureCake.PlutusCore.Evaluation.Machine.Exception
    ( UnliftingError (..)
    , MachineError (..)
    , AsMachineError (..)
    , EvaluationError (..)
    , AsEvaluationError (..)
    , ErrorWithCause (..)
    , EvaluationException
    , throwing_
    , throwingWithCause
    ) where

import PlutusPrelude

import PureCake.PlutusCore.Evaluation.Result

import Control.Lens
import Control.Monad.Error.Lens (throwing_)
import Control.Monad.Except
import Data.String (IsString)
import Data.Text (Text)

-- | When unlifting of a PLC term into a Haskell value fails, this error is thrown.
newtype UnliftingError
    = UnliftingErrorE Text
    deriving stock (Show, Eq)
    deriving newtype (IsString, Semigroup, NFData)

-- | The type of errors that 'readKnown' and 'makeKnown' can return.
data KnownTypeError
    = KnownTypeUnliftingError UnliftingError
    deriving stock (Eq)

-- | Errors which can occur during a run of an abstract machine.
data MachineError fun
    = NonPolymorphicInstantiationMachineError
      -- ^ An attempt to reduce a not immediately reducible type instantiation.
    | NonWrapUnwrappedMachineError
      -- ^ An attempt to unwrap a not wrapped term.
    | NonFunctionalApplicationMachineError
      -- ^ An attempt to reduce a not immediately reducible application.
    | OpenTermEvaluatedMachineError
      -- ^ An attempt to evaluate an open term.
    | UnliftingMachineError UnliftingError
      -- ^ An attempt to compute a constant application resulted in 'ConstAppError'.
    | BuiltinTermArgumentExpectedMachineError
      -- ^ A builtin expected a term argument, but something else was received
    | UnexpectedBuiltinTermArgumentMachineError
      -- ^ A builtin received a term argument when something else was expected
    | UnknownBuiltin fun
    deriving stock (Show, Eq, Functor, Generic)

-- | The type of errors (all of them) which can occur during evaluation
-- (some are used-caused, some are internal).
data EvaluationError user internal
    = InternalEvaluationError internal
      -- ^ Indicates bugs.
    | UserEvaluationError user
      -- ^ Indicates user errors.
    deriving stock (Show, Eq, Functor, Generic)

mtraverse makeClassyPrisms
    [ ''UnliftingError
    , ''MachineError
    , ''EvaluationError
    ]

instance internal ~ MachineError fun => AsMachineError (EvaluationError user internal) fun where
    _MachineError = _InternalEvaluationError
instance AsEvaluationFailure user => AsEvaluationFailure (EvaluationError user internal) where
    _EvaluationFailure = _UserEvaluationError . _EvaluationFailure

-- | An error and (optionally) what caused it.
data ErrorWithCause err cause = ErrorWithCause
    { _ewcError :: err
    , _ewcCause :: Maybe cause
    } deriving stock (Eq, Functor, Foldable, Traversable, Generic, Show)
    deriving anyclass (NFData)

instance AsEvaluationFailure err => AsEvaluationFailure (ErrorWithCause err cause) where
    _EvaluationFailure = iso _ewcError (flip ErrorWithCause Nothing) . _EvaluationFailure

type EvaluationException user internal =
    ErrorWithCause (EvaluationError user internal)

-- | "Prismatically" throw an error and its (optional) cause.
throwingWithCause
    -- Binds exc so it can be used as a convenient parameter with TypeApplications
    :: forall exc e t term m x
    . (exc ~ ErrorWithCause e term, MonadError exc m)
    => AReview e t -> t -> Maybe term -> m x
throwingWithCause l t cause = reviews l (\e -> throwError $ ErrorWithCause e cause) t

deriving anyclass instance
    (Show (ErrorWithCause err cause), Typeable cause, Typeable err) => Exception (ErrorWithCause err cause)
