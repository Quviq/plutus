{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE PolyKinds      #-}
module PureCake.ToPureCake where

import Data.Coerce

import PlutusCore.Default.Builtins qualified as PLC
import PlutusCore.Evaluation.Machine.Exception qualified as PLC
import PlutusCore.Evaluation.Machine.ExMemory qualified as PLC

import PureCake.PlutusCore.Default.Builtins qualified as Cake
import PureCake.PlutusCore.Evaluation.Machine.Exception qualified as Cake
import PureCake.PlutusCore.Evaluation.Machine.ExMemory qualified as Cake
import PureCake.UntypedPlutusCore.Core qualified as Cake

import PlutusCore.DeBruijn.Internal qualified as PLC
import PlutusCore.Default.Universe qualified as PLC
import PlutusCore.Evaluation.Machine.ExBudget qualified as PLC
import UntypedPlutusCore.Core qualified as PLC
import UntypedPlutusCore.Evaluation.Machine.Cek.ExBudgetMode qualified as PLC
import UntypedPlutusCore.Evaluation.Machine.Cek.Internal qualified as PLC

import PureCake.PlutusCore.DeBruijn.Internal qualified as Cake
import PureCake.PlutusCore.Default.Universe qualified as Cake
import PureCake.PlutusCore.Evaluation.Machine.ExBudget qualified as Cake
import PureCake.UntypedPlutusCore.Evaluation.Machine.Cek.ExBudgetMode qualified as Cake
import PureCake.UntypedPlutusCore.Evaluation.Machine.Cek.Internal qualified as Cake

countingStToCake :: PLC.CountingSt -> Cake.CountingSt
countingStToCake (PLC.CountingSt exBudget) = Cake.CountingSt (exBudgetToCake exBudget)

restrictingStToCake :: PLC.RestrictingSt -> Cake.RestrictingSt
restrictingStToCake (PLC.RestrictingSt exBudget) =
  Cake.RestrictingSt (exRestrictingBudgetToCake exBudget)

cekExceptionToCake :: PLC.CekEvaluationException PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun
                   -> Cake.CekEvaluationException Cake.NamedDeBruijn Cake.DefaultUni Cake.DefaultFun
cekExceptionToCake (PLC.ErrorWithCause e mc) =
  Cake.ErrorWithCause (evaluationErrorToCake e) (termToCake <$> mc)

evaluationErrorToCake :: PLC.EvaluationError PLC.CekUserError (PLC.MachineError PLC.DefaultFun)
                      -> Cake.EvaluationError Cake.CekUserError (Cake.MachineError Cake.DefaultFun)
evaluationErrorToCake (PLC.InternalEvaluationError i) =
  Cake.InternalEvaluationError (machineErrorToCake i)
evaluationErrorToCake (PLC.UserEvaluationError e) =
  Cake.UserEvaluationError (userErrorToCake e)

userErrorToCake :: PLC.CekUserError -> Cake.CekUserError
userErrorToCake (PLC.CekOutOfExError e) =
  Cake.CekOutOfExError (coerce . exBudgetToCake . coerce $ e)
userErrorToCake PLC.CekEvaluationFailure = Cake.CekEvaluationFailure

machineErrorToCake :: PLC.MachineError PLC.DefaultFun
                   -> Cake.MachineError Cake.DefaultFun
machineErrorToCake = \case
  PLC.NonPolymorphicInstantiationMachineError   -> Cake.NonPolymorphicInstantiationMachineError
  PLC.NonWrapUnwrappedMachineError              -> Cake.NonWrapUnwrappedMachineError
  PLC.NonFunctionalApplicationMachineError      -> Cake.NonFunctionalApplicationMachineError
  PLC.OpenTermEvaluatedMachineError             -> Cake.OpenTermEvaluatedMachineError
  PLC.UnliftingMachineError e                   -> Cake.UnliftingMachineError
                                                          (unliftingErrorToCake e)
  PLC.BuiltinTermArgumentExpectedMachineError   -> Cake.BuiltinTermArgumentExpectedMachineError
  PLC.UnexpectedBuiltinTermArgumentMachineError -> Cake.UnexpectedBuiltinTermArgumentMachineError
  PLC.UnknownBuiltin fun                        -> Cake.UnknownBuiltin (funToCake fun)

unliftingErrorToCake :: PLC.UnliftingError -> Cake.UnliftingError
unliftingErrorToCake = coerce

exRestrictingBudgetToCake :: PLC.ExRestrictingBudget -> Cake.ExRestrictingBudget
exRestrictingBudgetToCake = coerce . exBudgetToCake . coerce

exBudgetToCake :: PLC.ExBudget -> Cake.ExBudget
exBudgetToCake (PLC.ExBudget cpu mem) = Cake.ExBudget (coerce cpu) (coerce mem)

termToCake :: PLC.Term  PLC.NamedDeBruijn  PLC.DefaultUni PLC.DefaultFun ann
           -> Cake.Term Cake.NamedDeBruijn Cake.DefaultUni Cake.DefaultFun ann
termToCake = \ case
  PLC.Var ann name      -> Cake.Var ann (nameToCake name)
  PLC.LamAbs ann name t -> Cake.LamAbs ann (nameToCake name) (termToCake t)
  PLC.Apply ann s t     -> Cake.Apply ann (termToCake s) (termToCake t)
  PLC.Force ann t       -> Cake.Force ann (termToCake t)
  PLC.Delay ann t       -> Cake.Delay ann (termToCake t)
  PLC.Constant ann c    -> Cake.Constant ann (constToCake c)
  PLC.Builtin ann fun   -> Cake.Builtin ann (funToCake fun)
  PLC.Error ann         -> Cake.Error ann

funToCake :: PLC.DefaultFun -> Cake.DefaultFun
funToCake = toEnum . fromEnum

constToCake :: PLC.Some (PLC.ValueOf PLC.DefaultUni) -> Cake.Some (Cake.ValueOf Cake.DefaultUni)
constToCake (PLC.Some val) = Cake.Some (valToCake val)

valToCake :: PLC.ValueOf PLC.DefaultUni a -> Cake.ValueOf Cake.DefaultUni a
valToCake (PLC.ValueOf uni a) = Cake.ValueOf (uniToCake uni) a

uniToCake :: forall k (a :: k). PLC.DefaultUni (PLC.Esc a) -> Cake.DefaultUni (Cake.Esc a)
uniToCake = \ case
    PLC.DefaultUniInteger    -> Cake.DefaultUniInteger
    PLC.DefaultUniByteString -> Cake.DefaultUniByteString
    PLC.DefaultUniString     -> Cake.DefaultUniString
    PLC.DefaultUniUnit       -> Cake.DefaultUniUnit
    PLC.DefaultUniBool       -> Cake.DefaultUniBool
    PLC.DefaultUniProtoList  -> Cake.DefaultUniProtoList
    PLC.DefaultUniProtoPair  -> Cake.DefaultUniProtoPair
    PLC.DefaultUniApply f a  -> Cake.DefaultUniApply (uniToCake f) (uniToCake a)
    PLC.DefaultUniData       -> Cake.DefaultUniData

nameToCake :: PLC.NamedDeBruijn -> Cake.NamedDeBruijn
nameToCake (PLC.NamedDeBruijn str (PLC.Index ix)) = Cake.NamedDeBruijn str (Cake.Index ix)

