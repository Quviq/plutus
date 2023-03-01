{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module PureCake.UntypedPlutusCore.Core
    ( TPLC.UniOf
    , TPLC.Version (..)
    , TPLC.Binder (..)
    , Term (..)
    , Program (..)
    ) where

import PlutusPrelude

import PureCake.PlutusCore.Core qualified as TPLC
import PureCake.PlutusCore.Name qualified as TPLC
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
    -- This is the cutoff at which constructors won't get pointer tags
    -- See Note [Term constructor ordering and numbers]
    | Error !ann
    deriving stock (Show, Functor, Generic, Eq)
    deriving anyclass (NFData)

-- | A 'Program' is simply a 'Term' coupled with a 'Version' of the core language.
data Program name uni fun ann = Program
    { _progAnn  :: ann
    , _progVer  :: TPLC.Version ann
    , _progTerm :: Term name uni fun ann
    }
    deriving stock (Show, Functor, Generic)
    deriving anyclass (NFData)

type instance TPLC.UniOf (Term name uni fun ann) = uni

type instance TPLC.HasUniques (Term name uni fun ann) = TPLC.HasUnique name TPLC.TermUnique
