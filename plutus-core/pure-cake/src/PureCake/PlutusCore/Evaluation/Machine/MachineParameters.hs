module PureCake.PlutusCore.Evaluation.Machine.MachineParameters where

import PureCake.PlutusCore.Builtin
import PureCake.UntypedPlutusCore.Core
import PureCake.PlutusCore.Evaluation.Machine.Exception
import PureCake.PlutusCore.Evaluation.Machine.ExBudget
import PureCake.UntypedPlutusCore.Evaluation.Machine.Cek.Internal
import PureCake.UntypedPlutusCore.Evaluation.Machine.Cek.CekMachineCosts (CekMachineCosts (..))

mkMachineParameters :: CekMachineCosts -> MachineParameters
mkMachineParameters mchnCosts = MachineParameters mchnCosts defaultRuntime

defaultRuntime :: BuiltinsRuntime DefaultFun CekValue
defaultRuntime = BuiltinsRuntime go
  where
    -- TODO: the budget here is liable to change once the tests start failing!
    -- Also, I have no clue if this is actually right or if we need to use the
    -- other constructors from CekValue as well here??
    go AddInteger =
      BuiltinExpectArgument $ \ c -> case c of
        VCon (ConstInteger i) ->
          BuiltinExpectArgument $ \ c' -> case c' of
            VCon (ConstInteger j) ->
              BuiltinResult (ExBudget 0 0) (MakeKnownSuccess (VCon $ ConstInteger $ i + j))
            _ -> BuiltinResult (ExBudget 0 0) (MakeKnownFailure [] BuiltinTermArgumentExpectedMachineError)
        _ -> BuiltinResult (ExBudget 0 0) (MakeKnownFailure [] BuiltinTermArgumentExpectedMachineError)
