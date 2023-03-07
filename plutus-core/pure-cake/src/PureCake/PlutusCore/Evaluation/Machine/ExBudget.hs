module PureCake.PlutusCore.Evaluation.Machine.ExBudget
    ( ExBudget(..)
    , minusExBudget
    , ExRestrictingBudget(..)
    )
where

import PureCake.PlutusCore.Evaluation.Machine.ExMemory

import Data.Semigroup

data ExBudget = ExBudget { exBudgetCPU :: ExCPU, exBudgetMemory :: ExMemory }
    deriving stock (Eq, Show)

-- | Subract one 'ExBudget' from another. Does not guarantee that the result is positive.
minusExBudget :: ExBudget -> ExBudget -> ExBudget
minusExBudget (ExBudget c1 m1) (ExBudget c2 m2) = ExBudget (c1-c2) (m1-m2)

-- These functions are performance critical, so we can't use GenericSemigroupMonoid, and we insist that they be inlined.
instance Semigroup ExBudget where
    (ExBudget cpu1 mem1) <> (ExBudget cpu2 mem2) = ExBudget (cpu1 <> cpu2) (mem1 <> mem2)
    stimes r (ExBudget (ExCPU cpu) (ExMemory mem)) = ExBudget (ExCPU (fromIntegral r * cpu))
                                                              (ExMemory (fromIntegral r * mem))

newtype ExRestrictingBudget = ExRestrictingBudget
    { unExRestrictingBudget :: ExBudget
    } deriving stock (Show, Eq)
