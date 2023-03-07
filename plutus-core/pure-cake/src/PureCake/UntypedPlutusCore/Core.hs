{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE EmptyDataDeriving  #-}

module PureCake.UntypedPlutusCore.Core
    ( Version (..)
    , Binder (..)
    , Term (..)
    , Const(..)
    , DefaultFun
    ) where

import PlutusPrelude

import Data.ByteString

import PureCake.PlutusCore.DeBruijn

-- Making all the fields strict gives us a couple of percent in benchmarks
-- See Note [Term constructor ordering and numbers]
data Term
    = Var NamedDeBruijn
    | LamAbs NamedDeBruijn Term
    | Apply Term Term
    | Force Term
    | Delay Term
    | Constant Const
    | Builtin DefaultFun
    | Error
    deriving stock (Show, Eq)

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

data DefaultFun
  deriving stock (Ord, Eq, Show)
