-- editorconfig-checker-disable-file
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE EmptyCase   #-}

module PureCake.PlutusCore.Evaluation.Machine.MachineParameters
where

import PureCake.PlutusCore.Default.Builtins

import PureCake.PlutusCore.Builtin

import GHC.Generics

data MachineParameters machinecosts val =
    MachineParameters {
      machineCosts    :: machinecosts
    , builtinsRuntime :: BuiltinsRuntime DefaultFun val
    }
    deriving stock Generic

mkMachineParameters :: machinecosts -> MachineParameters machinecosts val
mkMachineParameters mchnCosts = MachineParameters mchnCosts (BuiltinsRuntime $ \case)
