{-# LANGUAGE EmptyDataDeriving  #-}

module PureCake.UntypedPlutusCore.Core
    ( Version (..)
    , Binder (..)
    , Term (..)
    , Const(..)
    , DefaultFun (..)
    ) where

import PlutusPrelude

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

data Version
    = Version Natural Natural Natural
    deriving stock (Eq, Show)

newtype Binder = Binder { unBinder :: NamedDeBruijn }

data Const =
    ConstInteger Integer
  | ConstString String
  | ConstBool Bool
  | ConstByteString ByteString
  | ConstUnit
  | ConstPair Const Const
  | ConstList [Const]
  deriving stock (Eq, Show)

data DefaultFun = AddInteger
  deriving stock (Ord, Eq, Show)
