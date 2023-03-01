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
      Name (..)
    , TyName (..)
    , Unique (..)
    , TypeUnique (..)
    , TermUnique (..)
    , HasUnique (..)
    , theUnique
    , UniqueMap (..)
    , insertByName
    , lookupName
    ) where

import PlutusPrelude

import Control.Lens
import Data.Hashable
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Text qualified as T
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift)

-- | A 'Name' represents variables/names in Plutus Core.
data Name = Name
    { _nameText   :: T.Text -- ^ The identifier name, for use in error messages.
    , _nameUnique :: Unique -- ^ A 'Unique' assigned to the name, allowing for cheap comparisons in the compiler.
    }
    deriving stock (Show, Generic, Lift)
    deriving anyclass (NFData, Hashable)

-- | We use a @newtype@ to enforce separation between names used for types and
-- those used for terms.
newtype TyName = TyName { unTyName :: Name }
    deriving stock (Show, Generic, Lift)
    deriving newtype (Eq, Ord, NFData, Hashable)
instance Wrapped TyName

data Named a = Named
    { _namedString :: Text
    , _namedValue  :: a
    } deriving stock (Functor, Foldable, Traversable)

instance Eq Name where
    (==) = (==) `on` _nameUnique

instance Ord Name where
    (<=) = (<=) `on` _nameUnique

-- | A unique identifier
newtype Unique = Unique { unUnique :: Int }
    deriving stock (Eq, Show, Ord, Lift)
    deriving newtype (Enum, NFData, Hashable)

-- | The unique of a type-level name.
newtype TypeUnique = TypeUnique
    { unTypeUnique :: Unique
    } deriving stock (Eq, Ord)
    deriving newtype Hashable

-- | The unique of a term-level name.
newtype TermUnique = TermUnique
    { unTermUnique :: Unique
    } deriving stock (Eq, Ord)
    deriving newtype Hashable

makeLenses 'Name

-- | Types which have a textual name attached to them.
class HasText a where
    theText :: Lens' a Text

instance HasText Name where
    theText = nameText

instance HasText TyName where
    theText = coerced . theText @Name

-- | Types which have a 'Unique' attached to them, mostly names.
class Coercible unique Unique => HasUnique a unique | a -> unique where
    unique :: Lens' a unique
    -- | The default implementation of 'HasUnique' for newtypes.
    default unique
        :: (Wrapped a, HasUnique (Unwrapped a) unique', Coercible unique' unique)
        => Lens' a unique
    unique = _Wrapped' . unique . coerced

instance HasUnique Unique Unique where
    unique = id

instance HasUnique Name TermUnique where
    unique = nameUnique . coerced

instance HasUnique TyName TypeUnique

-- | A lens focused on the 'Unique' of a name.
theUnique :: HasUnique name unique => Lens' name Unique
theUnique = unique . coerced

-- | A mapping from uniques to values of type @a@.
newtype UniqueMap unique a = UniqueMap
    { unUniqueMap :: IM.IntMap a
    } deriving newtype (Show, Eq, Semigroup, Monoid, Functor)

-- | Insert a value by a unique.
insertByUnique :: Coercible unique Unique => unique -> a -> UniqueMap unique a -> UniqueMap unique a
insertByUnique uniq = coerce . IM.insert (coerce uniq)

-- | Insert a value by the unique of a name.
insertByName :: HasUnique name unique => name -> a -> UniqueMap unique a -> UniqueMap unique a
insertByName = insertByUnique . view unique

-- | Look up a value by a unique.
lookupUnique :: Coercible unique Unique => unique -> UniqueMap unique a -> Maybe a
lookupUnique uniq = IM.lookup (coerce uniq) . unUniqueMap

-- | Look up a value by the unique of a name.
lookupName :: HasUnique name unique => name -> UniqueMap unique a -> Maybe a
lookupName = lookupUnique . view unique
