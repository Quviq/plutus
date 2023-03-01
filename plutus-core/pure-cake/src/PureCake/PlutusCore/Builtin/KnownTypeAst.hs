{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DefaultSignatures        #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

module PureCake.PlutusCore.Builtin.KnownTypeAst
    ( TyNameRep (..)
    , Hole
    , RepHole
    , TypeHole
    , KnownBuiltinTypeAst
    , KnownTypeAst (..)
    ) where

import PureCake.PlutusCore.Builtin.Emitter
import PureCake.PlutusCore.Builtin.KnownKind
import PureCake.PlutusCore.Builtin.Polymorphism
import PureCake.PlutusCore.Evaluation.Result
import PureCake.PlutusCore.Name

import Data.Kind qualified as GHC (Constraint, Type)
import Data.Proxy
import Data.Some.GADT qualified as GADT
import Data.Text qualified as Text
import Data.Type.Bool
import GHC.TypeLits
import Universe

import Prelude hiding (error)

import PureCake.PlutusCore.Core

-- See Note [Rep vs Type context].
-- | The kind of holes.
data Hole

-- See Note [Rep vs Type context].
-- | A hole in the Rep context.
type RepHole :: forall a hole. a -> hole
data family RepHole x

-- See Note [Rep vs Type context].
-- | A hole in the Type context.
type TypeHole :: forall hole. GHC.Type -> hole
data family TypeHole a

-- | For annotating an uninstantiated built-in type, so that it gets handled by the right instance
-- or type family.
type BuiltinHead :: forall k. k -> k
data family BuiltinHead f

type ElaborateBuiltin :: forall k. k -> k
type family ElaborateBuiltin a where
    ElaborateBuiltin (f x) = ElaborateBuiltin f `TyAppRep` x
    ElaborateBuiltin f     = BuiltinHead f

-- | A constraint for \"@a@ is a 'KnownTypeAst' by means of being included in @uni@\".
type KnownBuiltinTypeAst uni a = KnownTypeAst uni (ElaborateBuiltin a)

type KnownTypeAst :: forall a. (GHC.Type -> GHC.Type) -> a -> GHC.Constraint
class KnownTypeAst uni x where
    -- | Whether @x@ is a built-in type.
    type IsBuiltin x :: Bool
    type IsBuiltin x = IsBuiltin (ElaborateBuiltin x)

    -- | Return every part of the type that can be a to-be-instantiated type variable.
    -- For example, in @Integer@ there's no such types and in @(a, b)@ it's the two arguments
    -- (@a@ and @b@) and the same applies to @a -> b@ (to mention a type that is not built-in).
    type ToHoles x :: [Hole]
    type ToHoles x = ToHoles (ElaborateBuiltin x)

    -- | Collect all unique variables (a variable consists of a textual name, a unique and a kind)
    -- in an @a@.
    type ToBinds x :: [GADT.Some TyNameRep]
    type ToBinds x = ToBinds (ElaborateBuiltin x)

    -- | The type representing @a@ used on the PLC side.
    toTypeAst :: proxy x -> Type TyName uni ()
    default toTypeAst :: KnownBuiltinTypeAst uni x => proxy x -> Type TyName uni ()
    toTypeAst _ = toTypeAst $ Proxy @(ElaborateBuiltin x)
    {-# INLINE toTypeAst #-}

instance KnownTypeAst uni a => KnownTypeAst uni (EvaluationResult a) where
    type IsBuiltin (EvaluationResult a) = 'False
    type ToHoles (EvaluationResult a) = '[TypeHole a]
    type ToBinds (EvaluationResult a) = ToBinds a
    toTypeAst _ = toTypeAst $ Proxy @a
    {-# INLINE toTypeAst #-}

instance KnownTypeAst uni a => KnownTypeAst uni (Emitter a) where
    type IsBuiltin (Emitter a) = 'False
    type ToHoles (Emitter a) = '[TypeHole a]
    type ToBinds (Emitter a) = ToBinds a
    toTypeAst _ = toTypeAst $ Proxy @a
    {-# INLINE toTypeAst #-}

instance KnownTypeAst uni rep => KnownTypeAst uni (SomeConstant uni rep) where
    type IsBuiltin (SomeConstant uni rep) = 'False
    type ToHoles (SomeConstant _ rep) = '[RepHole rep]
    type ToBinds (SomeConstant _ rep) = ToBinds rep
    toTypeAst _ = toTypeAst $ Proxy @rep
    {-# INLINE toTypeAst #-}

instance KnownTypeAst uni rep => KnownTypeAst uni (Opaque val rep) where
    type IsBuiltin (Opaque val rep) = 'False
    type ToHoles (Opaque _ rep) = '[RepHole rep]
    type ToBinds (Opaque _ rep) = ToBinds rep
    toTypeAst _ = toTypeAst $ Proxy @rep
    {-# INLINE toTypeAst #-}

toTyNameAst
    :: forall text uniq. (KnownSymbol text, KnownNat uniq)
    => Proxy ('TyNameRep text uniq) -> TyName
toTyNameAst _ =
    TyName $ Name
        (Text.pack $ symbolVal @text Proxy)
        (Unique . fromIntegral $ natVal @uniq Proxy)
{-# INLINE toTyNameAst #-}

instance uni `Contains` f => KnownTypeAst uni (BuiltinHead f) where
    type IsBuiltin (BuiltinHead f) = 'True
    type ToHoles (BuiltinHead f) = '[]
    type ToBinds (BuiltinHead f) = '[]
    toTypeAst _ = mkTyBuiltin @_ @f ()
    {-# INLINE toTypeAst #-}

instance (KnownTypeAst uni a, KnownTypeAst uni b) => KnownTypeAst uni (a -> b) where
    type IsBuiltin (a -> b) = 'False
    type ToHoles (a -> b) = '[TypeHole a, TypeHole b]
    type ToBinds (a -> b) = Merge (ToBinds a) (ToBinds b)
    toTypeAst _ = TyFun () (toTypeAst $ Proxy @a) (toTypeAst $ Proxy @b)
    {-# INLINE toTypeAst #-}

instance (name ~ 'TyNameRep text uniq, KnownSymbol text, KnownNat uniq) =>
            KnownTypeAst uni (TyVarRep name) where
    type IsBuiltin (TyVarRep name) = 'False
    type ToHoles (TyVarRep name) = '[]
    type ToBinds (TyVarRep name) = '[ 'GADT.Some name ]
    toTypeAst _ = TyVar () . toTyNameAst $ Proxy @('TyNameRep text uniq)
    {-# INLINE toTypeAst #-}

instance (KnownTypeAst uni fun, KnownTypeAst uni arg) => KnownTypeAst uni (TyAppRep fun arg) where
    type IsBuiltin (TyAppRep fun arg) = IsBuiltin fun && IsBuiltin arg
    type ToHoles (TyAppRep fun arg) = '[RepHole fun, RepHole arg]
    type ToBinds (TyAppRep fun arg) = Merge (ToBinds fun) (ToBinds arg)
    toTypeAst _ = TyApp () (toTypeAst $ Proxy @fun) (toTypeAst $ Proxy @arg)
    {-# INLINE toTypeAst #-}

-- Utils

-- | Delete all @x@s from a list.
type family Delete x xs :: [a] where
    Delete _ '[]       = '[]
    Delete x (x ': xs) = Delete x xs
    Delete x (y ': xs) = y ': Delete x xs

-- | Delete all elements appearing in the first list from the second one and concatenate the lists.
type family Merge xs ys :: [a] where
    Merge '[]       ys = ys
    Merge (x ': xs) ys = x ': Delete x (Merge xs ys)

mkTyBuiltinOf :: forall k (a :: k) uni tyname ann. ann -> uni (Esc a) -> Type tyname uni ann
mkTyBuiltinOf ann = TyBuiltin ann . SomeTypeIn

mkTyBuiltin
    :: forall k (a :: k) uni tyname ann. uni `Contains` a
    => ann -> Type tyname uni ann
mkTyBuiltin ann = mkTyBuiltinOf ann $ knownUni @_ @uni @a
