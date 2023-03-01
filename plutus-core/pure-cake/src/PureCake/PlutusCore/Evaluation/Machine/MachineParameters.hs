-- editorconfig-checker-disable-file
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module PureCake.PlutusCore.Evaluation.Machine.MachineParameters
where

import PureCake.PlutusCore.Default.Builtins
import PureCake.PlutusCore.Default.Universe

import PureCake.PlutusCore.Builtin

import Control.DeepSeq
import Control.Lens
import GHC.Exts (inline)
import GHC.Generics
import GHC.Types (Type)
import NoThunks.Class

data CostModel machinecosts builtincosts =
    CostModel {
      _machineCostModel :: machinecosts
    , _builtinCostModel :: builtincosts
    } deriving stock (Eq, Show)
makeLenses ''CostModel

data MachineParameters machinecosts val =
    MachineParameters {
      machineCosts    :: machinecosts
    , builtinsRuntime :: BuiltinsRuntime DefaultFun val
    }
    deriving stock Generic
    deriving anyclass (NFData, NoThunks)

mkMachineParameters ::
    ( CostingPart DefaultUni DefaultFun ~ builtincosts
    , HasMeaningIn DefaultUni val
    )
    => BuiltinVersion DefaultFun
    -> CostModel machinecosts builtincosts
    -> MachineParameters machinecosts val
mkMachineParameters ver (CostModel mchnCosts builtinCosts) =
    MachineParameters mchnCosts (inline toBuiltinsRuntime ver builtinCosts)
{-# INLINE mkMachineParameters #-}
