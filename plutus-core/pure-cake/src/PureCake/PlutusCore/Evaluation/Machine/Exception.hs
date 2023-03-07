{-# LANGUAGE DeriveAnyClass         #-}

module PureCake.PlutusCore.Evaluation.Machine.Exception
    ( UnliftingError (..)
    , MachineError (..)
    , EvaluationError (..)
    , ErrorWithCause (..)
    , CekUserError(..)
    , EvaluationResult (..)
    , throwingWithCause
    ) where

import PlutusPrelude

import PureCake.PlutusCore.Evaluation.Machine.ExBudget
import PureCake.UntypedPlutusCore.Core

import Control.Monad.Except
import Data.Text (Text)

data EvaluationResult a
    = EvaluationSuccess !a
    | EvaluationFailure
    deriving stock (Show, Eq, Functor)

-- | When unlifting of a PLC term into a Haskell value fails, this error is thrown.
newtype UnliftingError
    = UnliftingErrorE Text
    deriving stock (Show, Eq)

-- | The type of errors that 'readKnown' and 'makeKnown' can return.
data KnownTypeError
    = KnownTypeUnliftingError UnliftingError
    deriving stock (Eq)

-- | Errors which can occur during a run of an abstract machine.
data MachineError
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
    | UnknownBuiltin DefaultFun
    deriving stock (Show, Eq)

-- | The type of errors (all of them) which can occur during evaluation
-- (some are used-caused, some are internal).
data EvaluationError
    = InternalEvaluationError MachineError
      -- ^ Indicates bugs.
    | UserEvaluationError CekUserError
      -- ^ Indicates user errors.
    deriving stock (Show, Eq)

data CekUserError
    -- @plutus-errors@ prevents this from being strict. Not that it matters anyway.
    = CekOutOfExError ExRestrictingBudget -- ^ The final overspent (i.e. negative) budget.
    | CekEvaluationFailure -- ^ Error has been called or a builtin application has failed
    deriving stock (Show, Eq)

-- | An error and (optionally) what caused it.
data ErrorWithCause = ErrorWithCause
    { _ewcError :: EvaluationError
    , _ewcCause :: Maybe Term
    } deriving stock (Eq, Show)

throwingWithCause
    :: (MonadError ErrorWithCause m)
    => EvaluationError -> Maybe Term -> m x
throwingWithCause e cause = throwError $ ErrorWithCause e cause

deriving anyclass instance Exception ErrorWithCause
