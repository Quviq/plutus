{-# LANGUAGE EmptyDataDeriving  #-}

module PureCake.UntypedPlutusCore.Core where

import Data.ByteString
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

data Term
    = Var NamedDeBruijn
    | LamAbs NamedDeBruijn Term
    | Apply Term Term
    | Force Term
    | Delay Term
    | Constant Const
    | Builtin DefaultFun
    | Error
    deriving stock Show

newtype Binder = Binder { unBinder :: NamedDeBruijn }

data Const =
    ConstInteger Integer
  | ConstString String
  | ConstBool Bool
  | ConstByteString ByteString
  | ConstUnit
  | ConstPair Const Const
  | ConstList [Const]
  deriving stock Show

data DefaultFun = AddInteger
  deriving stock (Ord, Eq, Show)
