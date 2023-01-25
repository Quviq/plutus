{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-deriving-strategies #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module PureCake.PlutusCore.Core.Instance.Recursive
    ( -- * Base functors
      TermF (..)
    , TypeF (..)
    , KindF (..)
    ) where

import PlutusPrelude
import PureCake.PlutusCore.Core.Type

import Data.Functor.Foldable.TH

$(join <$> traverse makeBaseFunctor [''Kind, ''Type, ''Term])
