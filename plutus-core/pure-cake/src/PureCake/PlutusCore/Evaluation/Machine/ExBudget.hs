{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PureCake.PlutusCore.Evaluation.Machine.ExBudget
    ( ExBudget(..)
    , minusExBudget
    , ExBudgetBuiltin(..)
    , ExRestrictingBudget(..)
    , LowerIntialCharacter
    )
where

import PlutusPrelude hiding (toList)
import PureCake.PlutusCore.Evaluation.Machine.ExMemory

import Data.Char (toLower)
import Data.Semigroup
import Deriving.Aeson
import Language.Haskell.TH.Lift (Lift)


-- | This is used elsewhere to convert cost models into JSON objects where the
-- names of the fields are exactly the same as the names of the builtins.
data LowerIntialCharacter
instance StringModifier LowerIntialCharacter where
  getStringModifier ""       = ""
  getStringModifier (c : xs) = toLower c : xs

-- | A class for injecting a 'Builtin' into an @exBudgetCat@.
-- We need it, because the constant application machinery calls 'spendBudget' before reducing a
-- constant application and we want to be general over @exBudgetCat@ there, but still track the
-- built-in functions category, hence the ad hoc polymorphism.
class ExBudgetBuiltin fun exBudgetCat where
    exBudgetBuiltin :: fun -> exBudgetCat

data ExBudget = ExBudget { exBudgetCPU :: ExCPU, exBudgetMemory :: ExMemory }
    deriving stock (Eq, Show, Generic, Lift)
    deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier LowerIntialCharacter] ExBudget
    -- LowerIntialCharacter won't actually do anything here, but let's have it in case we change the field names.

-- | Subract one 'ExBudget' from another. Does not guarantee that the result is positive.
minusExBudget :: ExBudget -> ExBudget -> ExBudget
minusExBudget (ExBudget c1 m1) (ExBudget c2 m2) = ExBudget (c1-c2) (m1-m2)

-- These functions are performance critical, so we can't use GenericSemigroupMonoid, and we insist that they be inlined.
instance Semigroup ExBudget where
    {-# INLINE (<>) #-}
    (ExBudget cpu1 mem1) <> (ExBudget cpu2 mem2) = ExBudget (cpu1 <> cpu2) (mem1 <> mem2)
    -- This absolutely must be inlined so that the 'fromIntegral' calls can get optimized away, or it destroys performance
    {-# INLINE stimes #-}
    stimes r (ExBudget (ExCPU cpu) (ExMemory mem)) = ExBudget (ExCPU (fromIntegral r * cpu)) (ExMemory (fromIntegral r * mem))

instance Monoid ExBudget where
    mempty = ExBudget mempty mempty

newtype ExRestrictingBudget = ExRestrictingBudget
    { unExRestrictingBudget :: ExBudget
    } deriving stock (Show, Eq)
      deriving newtype (Semigroup, Monoid)
