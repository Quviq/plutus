module PureCake.PlutusCore.Builtin where

import PureCake.PlutusCore.Evaluation.Machine.ExBudget
import PureCake.PlutusCore.Evaluation.Machine.Exception

data MakeKnownM a
    = MakeKnownFailure [String] MachineError
    | MakeKnownSuccess a
    | MakeKnownSuccessWithLogs [String] a

data BuiltinRuntime val
    = BuiltinResult ExBudget (MakeKnownM val)
    | BuiltinExpectArgument (val -> BuiltinRuntime val)
    | BuiltinExpectForce (BuiltinRuntime val)

data BuiltinsRuntime fun val = BuiltinsRuntime
    { unBuiltinsRuntime :: fun -> BuiltinRuntime val
    }

data BuiltinMeaning val cost = BuiltinMeaning (cost -> BuiltinRuntime val)
