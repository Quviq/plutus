{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE PolyKinds      #-}
module PureCake.ToPureCake where

import Data.Coerce

import PlutusCore.Default qualified as PLC
import PlutusCore.Evaluation.Machine.Exception qualified as PLC
import PlutusCore.Evaluation.Machine.ExMemory qualified as PLC

import PureCake.PlutusCore.Default.Builtins qualified as Cake
import PureCake.PlutusCore.Evaluation.Machine.Exception qualified as Cake
import PureCake.PlutusCore.Evaluation.Machine.ExMemory qualified as Cake
import PureCake.UntypedPlutusCore.Core qualified as Cake

import PlutusCore.DeBruijn qualified as PLC
import PlutusCore.Evaluation.Machine.ExBudget qualified as PLC
import UntypedPlutusCore.Core qualified as PLC
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as PLC

import PureCake.PlutusCore.DeBruijn qualified as Cake
import PureCake.PlutusCore.Default.Universe qualified as Cake
import PureCake.PlutusCore.Evaluation.Machine.ExBudget qualified as Cake
import PureCake.UntypedPlutusCore.Evaluation.Machine.Cek.ExBudgetMode qualified as Cake
import PureCake.UntypedPlutusCore.Evaluation.Machine.Cek.Internal qualified as Cake

restrictingStToCake :: PLC.RestrictingSt -> Cake.RestrictingSt
restrictingStToCake (PLC.RestrictingSt exBudget) =
  Cake.RestrictingSt (exRestrictingBudgetToCake exBudget)

cekExceptionToCake :: PLC.CekEvaluationException PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun
                   -> Cake.CekEvaluationException
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

cakeToFun :: Cake.DefaultFun -> PLC.DefaultFun
cakeToFun = \case
  Cake.AddInteger               -> PLC.AddInteger
  Cake.SubtractInteger          -> PLC.SubtractInteger
  Cake.MultiplyInteger          -> PLC.MultiplyInteger
  Cake.DivideInteger            -> PLC.DivideInteger
  Cake.QuotientInteger          -> PLC.QuotientInteger
  Cake.RemainderInteger         -> PLC.RemainderInteger
  Cake.ModInteger               -> PLC.ModInteger
  Cake.EqualsInteger            -> PLC.EqualsInteger
  Cake.LessThanInteger          -> PLC.LessThanInteger
  Cake.LessThanEqualsInteger    -> PLC.LessThanEqualsInteger
  Cake.AppendByteString         -> PLC.AppendByteString
  Cake.ConsByteString           -> PLC.ConsByteString
  Cake.SliceByteString          -> PLC.SliceByteString
  Cake.LengthOfByteString       -> PLC.LengthOfByteString
  Cake.IndexByteString          -> PLC.IndexByteString
  Cake.EqualsByteString         -> PLC.EqualsByteString
  Cake.LessThanByteString       -> PLC.LessThanByteString
  Cake.LessThanEqualsByteString -> PLC.LessThanEqualsByteString
  Cake.AppendString             -> PLC.AppendString
  Cake.EqualsString             -> PLC.EqualsString
  Cake.ConstrData               -> PLC.ConstrData
  Cake.MapData                  -> PLC.MapData
  Cake.ListData                 -> PLC.ListData
  Cake.IData                    -> PLC.IData
  Cake.BData                    -> PLC.BData
  Cake.UnConstrData             -> PLC.UnConstrData
  Cake.UnMapData                -> PLC.UnMapData
  Cake.UnListData               -> PLC.UnListData
  Cake.UnIData                  -> PLC.UnIData
  Cake.UnBData                  -> PLC.UnBData
  Cake.EqualsData               -> PLC.EqualsData
  Cake.SerialiseData            -> PLC.SerialiseData

funToCake :: PLC.DefaultFun -> Cake.DefaultFun
funToCake = \case
  PLC.AddInteger               -> Cake.AddInteger
  PLC.SubtractInteger          -> Cake.SubtractInteger
  PLC.MultiplyInteger          -> Cake.MultiplyInteger
  PLC.DivideInteger            -> Cake.DivideInteger
  PLC.QuotientInteger          -> Cake.QuotientInteger
  PLC.RemainderInteger         -> Cake.RemainderInteger
  PLC.ModInteger               -> Cake.ModInteger
  PLC.EqualsInteger            -> Cake.EqualsInteger
  PLC.LessThanInteger          -> Cake.LessThanInteger
  PLC.LessThanEqualsInteger    -> Cake.LessThanEqualsInteger
  PLC.AppendByteString         -> Cake.AppendByteString
  PLC.ConsByteString           -> Cake.ConsByteString
  PLC.SliceByteString          -> Cake.SliceByteString
  PLC.LengthOfByteString       -> Cake.LengthOfByteString
  PLC.IndexByteString          -> Cake.IndexByteString
  PLC.EqualsByteString         -> Cake.EqualsByteString
  PLC.LessThanByteString       -> Cake.LessThanByteString
  PLC.LessThanEqualsByteString -> Cake.LessThanEqualsByteString
  PLC.AppendString             -> Cake.AppendString
  PLC.EqualsString             -> Cake.EqualsString
  PLC.ConstrData               -> Cake.ConstrData
  PLC.MapData                  -> Cake.MapData
  PLC.ListData                 -> Cake.ListData
  PLC.IData                    -> Cake.IData
  PLC.BData                    -> Cake.BData
  PLC.UnConstrData             -> Cake.UnConstrData
  PLC.UnMapData                -> Cake.UnMapData
  PLC.UnListData               -> Cake.UnListData
  PLC.UnIData                  -> Cake.UnIData
  PLC.UnBData                  -> Cake.UnBData
  PLC.EqualsData               -> Cake.EqualsData
  PLC.SerialiseData            -> Cake.SerialiseData

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

