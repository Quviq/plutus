module PureCake.PlutusCore.Evaluation.Machine.ExBudget
    ( ExBudget(..)
    , minusExBudget
    , stimesExBudget
    , ExRestrictingBudget(..)
    , CostingInteger
    , ExMemory
    , ExCPU
    )
where

import Data.SatInt

type CostingInteger = SatInt

-- | Counts size in machine words.
type ExMemory = CostingInteger

-- | Counts CPU units in picoseconds: maximum value for SatInt is 2^63 ps, or
-- appproximately 106 days.
type ExCPU = CostingInteger

data ExBudget = ExBudget { exBudgetCPU :: ExCPU, exBudgetMemory :: ExMemory }
    deriving stock (Eq, Show)

minusExBudget :: ExBudget -> ExBudget -> ExBudget
minusExBudget (ExBudget c1 m1) (ExBudget c2 m2) =
  ExBudget (c1 - c2) (m1 - m2)

stimesExBudget :: Integral i  => i -> ExBudget -> ExBudget
stimesExBudget r (ExBudget cpu mem) = ExBudget (fromIntegral r * cpu)
                                               (fromIntegral r * mem)

newtype ExRestrictingBudget = ExRestrictingBudget
    { unExRestrictingBudget :: ExBudget
    } deriving stock (Show, Eq)
