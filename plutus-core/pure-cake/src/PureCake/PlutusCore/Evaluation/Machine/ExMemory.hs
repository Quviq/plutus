module PureCake.PlutusCore.Evaluation.Machine.ExMemory
  ( CostingInteger
  , ExMemory(..)
  , ExCPU(..)
  , ExMemoryUsage(..)
  ) where

import Control.Monad.RWS.Strict
import Data.SatInt

type CostingInteger = SatInt

-- | Counts size in machine words.
newtype ExMemory = ExMemory CostingInteger
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num)
  deriving (Semigroup, Monoid) via (Sum CostingInteger)

-- | Counts CPU units in picoseconds: maximum value for SatInt is 2^63 ps, or
-- appproximately 106 days.
newtype ExCPU = ExCPU CostingInteger
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num)
  deriving (Semigroup, Monoid) via (Sum CostingInteger)

class ExMemoryUsage a where
    -- Inlining the implementations of this method gave us a 1-2% speedup.
    memoryUsage :: a -> ExMemory -- ^ How much memory does 'a' use?
