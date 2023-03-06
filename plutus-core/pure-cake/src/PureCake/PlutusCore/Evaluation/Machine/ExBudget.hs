{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PureCake.PlutusCore.Evaluation.Machine.ExBudget
    ( ExBudget(..)
    , minusExBudget
    , ExBudgetBuiltin(..)
    , ExRestrictingBudget(..)
    )
where

import PureCake.PlutusCore.Evaluation.Machine.ExMemory

import Data.Semigroup

-- | A class for injecting a 'Builtin' into an @exBudgetCat@.
-- We need it, because the constant application machinery calls 'spendBudget' before reducing a
-- constant application and we want to be general over @exBudgetCat@ there, but still track the
-- built-in functions category, hence the ad hoc polymorphism.
class ExBudgetBuiltin fun exBudgetCat where
    exBudgetBuiltin :: fun -> exBudgetCat

data ExBudget = ExBudget { exBudgetCPU :: ExCPU, exBudgetMemory :: ExMemory }
    deriving stock (Eq, Show)

-- | Subract one 'ExBudget' from another. Does not guarantee that the result is positive.
minusExBudget :: ExBudget -> ExBudget -> ExBudget
minusExBudget (ExBudget c1 m1) (ExBudget c2 m2) = ExBudget (c1-c2) (m1-m2)

-- These functions are performance critical, so we can't use GenericSemigroupMonoid, and we insist that they be inlined.
instance Semigroup ExBudget where
    (ExBudget cpu1 mem1) <> (ExBudget cpu2 mem2) = ExBudget (cpu1 <> cpu2) (mem1 <> mem2)
    stimes r (ExBudget (ExCPU cpu) (ExMemory mem)) = ExBudget (ExCPU (fromIntegral r * cpu)) (ExMemory (fromIntegral r * mem))

newtype ExRestrictingBudget = ExRestrictingBudget
    { unExRestrictingBudget :: ExBudget
    } deriving stock (Show, Eq)
