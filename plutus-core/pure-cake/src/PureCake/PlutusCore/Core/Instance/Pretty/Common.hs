{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

module PureCake.PlutusCore.Core.Instance.Pretty.Common () where

import PlutusPrelude

import PureCake.PlutusCore.Core.Type

instance Pretty (Version ann) where
    pretty (Version _ i j k) = pretty i <> "." <> pretty j <> "." <> pretty k
