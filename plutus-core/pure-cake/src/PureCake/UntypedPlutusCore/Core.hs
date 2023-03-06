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

import PureCake.PlutusCore.Evaluation.Machine.ExMemory

import Data.ByteString

-- Making all the fields strict gives us a couple of percent in benchmarks
-- See Note [Term constructor ordering and numbers]
data Term name
    = Var name
    | LamAbs name (Term name)
    | Apply (Term name) (Term name)
    | Force (Term name)
    | Delay (Term name)
    | Constant Const
    | Builtin DefaultFun
    | Error
    deriving stock (Show, Functor, Eq)

data Version ann
    = Version ann Natural Natural Natural
    deriving stock (Eq, Show, Functor)

newtype Binder name = Binder { unBinder :: name }

data Const =
    ConstInteger Integer
  | ConstString String
  | ConstBool Bool
  | ConstByteString ByteString
  | ConstUnit
  deriving (Eq, Show)

data DefaultFun
  deriving stock (Ord, Eq, Show)

instance ExMemoryUsage DefaultFun where
    memoryUsage _ = 1
