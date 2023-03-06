-- editorconfig-checker-disable-file
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}

module PureCake.PlutusCore.Name
    ( -- * Types
      Unique (..)
    , TypeUnique (..)
    , TermUnique (..)
    ) where

import PlutusPrelude

import Data.Hashable
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift)

-- | A unique identifier
newtype Unique = Unique { unUnique :: Int }
    deriving stock (Eq, Show, Ord, Lift)
    deriving newtype (Enum, NFData)

-- | The unique of a type-level name.
newtype TypeUnique = TypeUnique
    { unTypeUnique :: Unique
    } deriving stock (Eq, Ord)

-- | The unique of a term-level name.
newtype TermUnique = TermUnique
    { unTermUnique :: Unique
    } deriving stock (Eq, Ord)
