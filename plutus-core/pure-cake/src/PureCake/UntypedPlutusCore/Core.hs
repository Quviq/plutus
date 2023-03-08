{-# LANGUAGE EmptyDataDeriving  #-}

module PureCake.UntypedPlutusCore.Core
    (  Binder (..)
    , Term (..)
    , Const(..)
    , DefaultFun (..)
    ) where

import Data.ByteString

import PureCake.PlutusCore.DeBruijn

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
