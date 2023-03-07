module PureCake.PlutusCore.Builtin where

data BuiltinRuntime val = BuiltinRuntime

data BuiltinsRuntime fun val = BuiltinsRuntime
    { unBuiltinsRuntime :: fun -> BuiltinRuntime val
    }

data BuiltinMeaning val cost = BuiltinMeaning (cost -> BuiltinRuntime val)
