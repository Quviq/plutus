module PureCake.UntypedPlutusCore.Evaluation.Machine.Cek.CekMachineCosts
    ( CekMachineCosts(..)
    , defaultCekMachineCosts
    )
where

import PureCake.PlutusCore.Evaluation.Machine.ExBudget

-- | Costs for evaluating AST nodes.  Times should be specified in picoseconds, memory sizes in bytes.

data CekMachineCosts =
    CekMachineCosts {
      cekStartupCost :: ExBudget  -- General overhead
    , cekVarCost     :: ExBudget
    , cekConstCost   :: ExBudget
    , cekLamCost     :: ExBudget
    , cekDelayCost   :: ExBudget
    , cekForceCost   :: ExBudget
    , cekApplyCost   :: ExBudget
    , cekBuiltinCost :: ExBudget
    -- ^ Just the cost of evaluating a Builtin node, not the builtin itself.
    -- There's no entry for Error since we'll be exiting anyway; also, what would
    -- happen if calling 'Error' caused the budget to be exceeded?
    }
    deriving stock (Eq, Show)

-- | Default costs for CEK machine instructions.
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
