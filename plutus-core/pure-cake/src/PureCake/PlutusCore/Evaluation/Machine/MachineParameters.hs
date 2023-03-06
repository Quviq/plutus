{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE EmptyCase   #-}

module PureCake.PlutusCore.Evaluation.Machine.MachineParameters where

import PureCake.PlutusCore.Builtin
import PureCake.UntypedPlutusCore.Core

data MachineParameters machinecosts val =
    MachineParameters {
      machineCosts    :: machinecosts
    , builtinsRuntime :: BuiltinsRuntime DefaultFun val
    }

mkMachineParameters :: machinecosts -> MachineParameters machinecosts val
mkMachineParameters mchnCosts = MachineParameters mchnCosts (BuiltinsRuntime $ \case)
