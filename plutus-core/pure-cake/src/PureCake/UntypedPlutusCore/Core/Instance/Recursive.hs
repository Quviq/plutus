{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-deriving-strategies #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module PureCake.UntypedPlutusCore.Core.Instance.Recursive
    ( -- * Base functors
      TermF (..)
    ) where

import PureCake.UntypedPlutusCore.Core.Type

import Data.Functor.Foldable.TH

$(makeBaseFunctor ''Term)
