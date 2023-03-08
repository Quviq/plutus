module PureCake.UntypedPlutusCore.Evaluation.Machine.Cek.CekMachineCosts
    ( CekMachineCosts(..)
    , defaultCekMachineCosts
    )
where

import PureCake.PlutusCore.Evaluation.Machine.ExBudget

data CekMachineCosts =
    CekMachineCosts {
      cekStartupCost :: ExBudget
    , cekVarCost     :: ExBudget
    , cekConstCost   :: ExBudget
    , cekLamCost     :: ExBudget
    , cekDelayCost   :: ExBudget
    , cekForceCost   :: ExBudget
    , cekApplyCost   :: ExBudget
    , cekBuiltinCost :: ExBudget
    }

defaultCekMachineCosts :: CekMachineCosts
defaultCekMachineCosts =
  CekMachineCosts { cekStartupCost = ExBudget 100 100
                  , cekVarCost     = ExBudget 23000 100
                  , cekConstCost   = ExBudget 23000 100
                  , cekLamCost     = ExBudget 23000 100
                  , cekDelayCost   = ExBudget 23000 100
                  , cekForceCost   = ExBudget 23000 100
                  , cekApplyCost   = ExBudget 23000 100
                  , cekBuiltinCost = ExBudget 23000 100
                  }
