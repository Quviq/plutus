module PureCake.PlutusCore.DeBruijn
    ( Index (..)
    , NamedDeBruijn (..)
    , deBruijnInitIndex
    ) where

import Data.Word

-- | A relative index used for de Bruijn identifiers.
newtype Index = Index Word64
    deriving newtype Show

-- | The LamAbs index (for debruijn indices) and the starting level of DeBruijn monad
deBruijnInitIndex :: Index
deBruijnInitIndex = Index 0

-- The bangs gave us a speedup of 6%.
-- | A term name as a de Bruijn index.
data NamedDeBruijn = NamedDeBruijn { ndbnString :: !String, ndbnIndex :: !Index }
    deriving stock Show
