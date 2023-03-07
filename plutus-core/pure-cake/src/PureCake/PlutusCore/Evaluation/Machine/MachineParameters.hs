module PureCake.PlutusCore.Evaluation.Machine.MachineParameters where

import PureCake.PlutusCore.Builtin
import PureCake.UntypedPlutusCore.Core
import PureCake.PlutusCore.Evaluation.Machine.Exception
import PureCake.PlutusCore.Evaluation.Machine.ExBudget

data MachineParameters machinecosts val =
    MachineParameters {
      machineCosts    :: machinecosts
    , builtinsRuntime :: BuiltinsRuntime DefaultFun val
    }

-- TODO: this type is wrong!
mkMachineParameters :: machinecosts -> MachineParameters machinecosts Const
mkMachineParameters mchnCosts = MachineParameters mchnCosts defaultRuntime

defaultRuntime :: BuiltinsRuntime DefaultFun Const
defaultRuntime = BuiltinsRuntime go
  where
    -- TODO: the budget here is liable to change once the tests start failing!
    go AddInteger =
      BuiltinExpectArgument $ \ c -> case c of
        ConstInteger i ->
          BuiltinExpectArgument $ \ c' -> case c' of
            ConstInteger j ->
              BuiltinResult (ExBudget 0 0) (MakeKnownSuccess (ConstInteger $ i + j))
            _ -> BuiltinResult (ExBudget 0 0) (MakeKnownFailure [] BuiltinTermArgumentExpectedMachineError)
        _ -> BuiltinResult (ExBudget 0 0) (MakeKnownFailure [] BuiltinTermArgumentExpectedMachineError)
