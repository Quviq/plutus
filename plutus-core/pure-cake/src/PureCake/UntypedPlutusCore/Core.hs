{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module PureCake.UntypedPlutusCore.Core
    ( Version (..)
    , Binder (..)
    , Term (..)
    ) where

import PlutusPrelude

import Universe

-- Making all the fields strict gives us a couple of percent in benchmarks
-- See Note [Term constructor ordering and numbers]
data Term name uni fun ann
    = Var !ann !name
    | LamAbs !ann !name !(Term name uni fun ann)
    | Apply !ann !(Term name uni fun ann) !(Term name uni fun ann)
    | Force !ann !(Term name uni fun ann)
    | Delay !ann !(Term name uni fun ann)
    | Constant !ann !(Some (ValueOf uni))
    | Builtin !ann !fun
    | Error !ann
    deriving stock (Show, Functor, Eq)


data Version ann
    = Version ann Natural Natural Natural
    deriving stock (Eq, Show, Functor)

newtype Binder name = Binder { unBinder :: name }
