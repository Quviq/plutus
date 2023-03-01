-- editorconfig-checker-disable-file
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Support for using de Bruijn indices for term and type names.
module PureCake.PlutusCore.DeBruijn
    ( Index (..)
    , NamedDeBruijn (..)
    , deBruijnInitIndex
    ) where

import PureCake.PlutusCore.Name

import Control.Exception
import Control.Lens hiding (Index, Level, index, ix)

import Data.Text qualified as T
import Data.Word

import Control.DeepSeq (NFData)
import ErrorCode
import GHC.Generics

-- | A relative index used for de Bruijn identifiers.
newtype Index = Index Word64
    deriving stock (Generic)
    deriving newtype (Show, Num, Enum, Real, Integral, Eq, Ord, NFData)

-- | The LamAbs index (for debruijn indices) and the starting level of DeBruijn monad
deBruijnInitIndex :: Index
deBruijnInitIndex = 0

-- The bangs gave us a speedup of 6%.
-- | A term name as a de Bruijn index.
data NamedDeBruijn = NamedDeBruijn { ndbnString :: !T.Text, ndbnIndex :: !Index }
    deriving stock (Show, Generic)
    deriving anyclass NFData

-- | A wrapper around nameddebruijn that must hold the invariant of name=`fakeName`.
newtype FakeNamedDeBruijn = FakeNamedDeBruijn NamedDeBruijn
    deriving newtype (Show, Eq, NFData)

instance Eq NamedDeBruijn where
    -- ignoring actual names and only relying solely on debruijn indices
    (NamedDeBruijn _ ix1) == (NamedDeBruijn _ ix2) = ix1 == ix2

-- | A term name as a de Bruijn index, without the name string.
newtype DeBruijn = DeBruijn { dbnIndex :: Index }
    deriving stock (Show, Generic, Eq)
    deriving newtype (NFData)

-- | A type name as a de Bruijn index.
newtype NamedTyDeBruijn = NamedTyDeBruijn NamedDeBruijn
    deriving stock (Show, Generic)
    deriving newtype (NFData)
    -- ignoring actual names and only relying solely on debruijn indices
    deriving Eq via NamedDeBruijn
instance Wrapped NamedTyDeBruijn

-- | A type name as a de Bruijn index, without the name string.
newtype TyDeBruijn = TyDeBruijn DeBruijn
    deriving stock (Show, Generic)
    deriving newtype (NFData)
    deriving Eq via DeBruijn
instance Wrapped TyDeBruijn

class HasIndex a where
    index :: Lens' a Index

instance HasIndex NamedDeBruijn where
    index = lens g s where
        g = ndbnIndex
        s n i = n{ndbnIndex=i}

instance HasIndex DeBruijn where
    index = lens g s where
        g = dbnIndex
        s n i = n{dbnIndex=i}

instance HasIndex NamedTyDeBruijn where
    index = _Wrapped' . index

instance HasIndex TyDeBruijn where
    index = _Wrapped' . index

-- Converting from normal names to DeBruijn indices, and vice versa

-- | An absolute level in the program.
newtype Level = Level Integer deriving newtype (Eq, Ord, Num, Real, Enum, Integral)

-- | We cannot do a correct translation to or from de Bruijn indices if the program is not well-scoped.
-- So we throw an error in such a case.
data FreeVariableError
    = FreeUnique Unique
    | FreeIndex Index
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Exception, NFData)

makeClassyPrisms ''FreeVariableError

instance HasErrorCode FreeVariableError where
    errorCode  FreeIndex {}  = ErrorCode 23
    errorCode  FreeUnique {} = ErrorCode 22
