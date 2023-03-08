{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE EmptyCase      #-}
module PureCake.ToPureCake where

import Data.Coerce
import Data.Text

import PlutusCore.Default qualified as PLC
import PlutusCore.Evaluation.Machine.Exception qualified as PLC
import PlutusCore.Evaluation.Machine.ExMemory qualified as PLC
import PlutusCore.DeBruijn qualified as PLC
import PlutusCore.Evaluation.Machine.ExBudget qualified as PLC
import UntypedPlutusCore.Core qualified as PLC
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as PLC

import PureCake.PlutusCore.Evaluation.Machine.Exception qualified as Cake
import PureCake.UntypedPlutusCore.Core qualified as Cake
import PureCake.PlutusCore.DeBruijn qualified as Cake
import PureCake.PlutusCore.Evaluation.Machine.ExBudget qualified as Cake
import PureCake.UntypedPlutusCore.Evaluation.Machine.Cek.ExBudgetMode qualified as Cake

restrictingStToCake :: PLC.RestrictingSt -> Cake.RestrictingSt
restrictingStToCake (PLC.RestrictingSt exBudget) =
  Cake.RestrictingSt (exRestrictingBudgetToCake exBudget)

cekExceptionToCake :: PLC.CekEvaluationException PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun
                   -> Cake.ErrorWithCause
cekExceptionToCake (PLC.ErrorWithCause e mc) =
  Cake.ErrorWithCause (evaluationErrorToCake e) (termToCake <$> mc)

evaluationErrorToCake :: PLC.EvaluationError PLC.CekUserError (PLC.MachineError PLC.DefaultFun)
                      -> Cake.EvaluationError
evaluationErrorToCake (PLC.InternalEvaluationError i) =
  Cake.InternalEvaluationError (machineErrorToCake i)
evaluationErrorToCake (PLC.UserEvaluationError e) =
  Cake.UserEvaluationError (userErrorToCake e)

userErrorToCake :: PLC.CekUserError -> Cake.CekUserError
userErrorToCake (PLC.CekOutOfExError e) =
  Cake.CekOutOfExError (coerce . exBudgetToCake . coerce $ e)
userErrorToCake PLC.CekEvaluationFailure = Cake.CekEvaluationFailure

machineErrorToCake :: PLC.MachineError PLC.DefaultFun
                   -> Cake.MachineError
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

termToCake :: PLC.Term PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun ()
           -> Cake.Term
termToCake = \ case
  PLC.Var _ name      -> Cake.Var (nameToCake name)
  PLC.LamAbs _ name t -> Cake.LamAbs (nameToCake name) (termToCake t)
  PLC.Apply _ s t     -> Cake.Apply (termToCake s) (termToCake t)
  PLC.Force _ t       -> Cake.Force (termToCake t)
  PLC.Delay _ t       -> Cake.Delay (termToCake t)
  PLC.Constant _ c    -> Cake.Constant (constToCake c)
  PLC.Builtin _ fun   -> Cake.Builtin (funToCake fun)
  PLC.Error _         -> Cake.Error

cakeToFun :: Cake.DefaultFun -> PLC.DefaultFun
cakeToFun = \case
  Cake.AddInteger -> PLC.AddInteger

funToCake :: PLC.DefaultFun -> Cake.DefaultFun
funToCake PLC.AddInteger = Cake.AddInteger
funToCake fun            = error $ "You can't convert PLC function " ++ show fun ++ " to cake functions"

constToCake :: PLC.Some (PLC.ValueOf PLC.DefaultUni) -> Cake.Const
constToCake (PLC.Some val) = valToCake val

valToCake :: PLC.ValueOf PLC.DefaultUni a -> Cake.Const
valToCake (PLC.ValueOf uni a) = case uni of
  PLC.DefaultUniInteger                         -> Cake.ConstInteger a
  PLC.DefaultUniString                          -> Cake.ConstString (unpack a)
  PLC.DefaultUniBool                            -> Cake.ConstBool a
  PLC.DefaultUniUnit                            -> Cake.ConstUnit
  PLC.DefaultUniByteString                      -> Cake.ConstByteString a
  PLC.DefaultUniApply
    PLC.DefaultUniProtoList
    u -> Cake.ConstList (valToCake . PLC.ValueOf u <$> a)
  PLC.DefaultUniApply
    (PLC.DefaultUniApply PLC.DefaultUniProtoPair u1)
    u2 -> Cake.ConstPair (valToCake $ PLC.ValueOf u1 $ fst a) (valToCake $ PLC.ValueOf u2 $ snd a)
  u                                             -> error $ "we don't yet handle " ++ show u

nameToCake :: PLC.NamedDeBruijn -> Cake.NamedDeBruijn
nameToCake (PLC.NamedDeBruijn str (PLC.Index ix)) = Cake.NamedDeBruijn (unpack str) (Cake.Index ix)

