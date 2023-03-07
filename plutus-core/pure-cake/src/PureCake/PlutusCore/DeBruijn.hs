module PureCake.PlutusCore.DeBruijn
    ( Index (..)
    , NamedDeBruijn (..)
    , deBruijnInitIndex
    ) where

import Data.Text qualified as T
import Data.Word

-- | A relative index used for de Bruijn identifiers.
newtype Index = Index Word64
    deriving newtype (Show, Eq)

-- | The LamAbs index (for debruijn indices) and the starting level of DeBruijn monad
deBruijnInitIndex :: Index
deBruijnInitIndex = Index 0

-- The bangs gave us a speedup of 6%.
-- | A term name as a de Bruijn index.
data NamedDeBruijn = NamedDeBruijn { ndbnString :: !T.Text, ndbnIndex :: !Index }
    deriving stock (Show)

instance Eq NamedDeBruijn where
    -- ignoring actual names and only relying solely on debruijn indices
    (NamedDeBruijn _ ix1) == (NamedDeBruijn _ ix2) = ix1 == ix2
