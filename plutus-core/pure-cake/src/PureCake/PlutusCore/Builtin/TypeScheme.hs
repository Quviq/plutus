-- editorconfig-checker-disable-file
-- | This module assigns types to built-ins.
-- See the @plutus/plutus-core/docs/Constant application.md@
-- article for how this emerged.

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

{-# LANGUAGE StrictData       #-}

module PureCake.PlutusCore.Builtin.TypeScheme
    ( TypeScheme (..)
    ) where

import PureCake.PlutusCore.Builtin.KnownType
import PureCake.PlutusCore.Builtin.KnownTypeAst
import PureCake.PlutusCore.Core

import Data.Kind qualified as GHC (Type)
import Data.Proxy
import GHC.TypeLits
import Type.Reflection

infixr 9 `TypeSchemeArrow`

{- Note [MakeKnown in TypeSchemeArrow]
There's a @MakeKnown val arg@ constrained packed in the 'TypeSchemeArrow' constructor. It's not
supposed to be there, but unfortunately, in the @Generators@ tests we use 'TypeScheme' for
generation of arbitrary arguments of builtins and that requires 'makeKnown', which makes us have
the 'MakeKnown' in 'TypeSchemeArrow'.

The solution is to fix the @Generators@ tests. Explicitly constraining @args@ outside of
'TypeScheme' sounds like a promising strategy. Maybe we could just delete those tests altogether.

However it's also worth considering untangling 'RuntimeScheme' from 'TypeScheme' and generating the
two in parallel, so that we only need to optimize the former. Then we will be able to afford having
any kind of nonsense in 'TypeScheme'. Another reason for that would be the fact that Core output
has 'typeSchemeToRuntimeScheme' all over the place as it can't be inlined due to being a recursive
function, which we can't turn into an inlinable class method, because the indices of 'TypeScheme'
don't reflect its structure due to the 'TypeSchemeAll' constructor not being reflected at the type
level in any way. It's unlikely that having those 'typeSchemeToRuntimeScheme' has any impact on
performance, because they're only evaluated once during initialization, but it certainly has impact
on readability of the Core output.
-}

-- We have these 'Typeable' constraints here just for the generators tests. It's fine since
-- 'TypeScheme' is not used for evaluation and so we can shove into 'TypeScheme' whatever we want.
-- | Type schemes of primitive operations.
-- @as@ is a list of types of arguments, @r@ is the resulting type.
-- E.g. @Text -> Bool -> Integer@ is encoded as @TypeScheme val [Text, Bool] Integer@.
data TypeScheme val (args :: [GHC.Type]) res where
    TypeSchemeResult
        :: (Typeable res, KnownTypeAst (UniOf val) res, MakeKnown val res)
        => TypeScheme val '[] res
    TypeSchemeArrow
        :: (Typeable arg, KnownTypeAst (UniOf val) arg, MakeKnown val arg, ReadKnown val arg)
        => TypeScheme val args res -> TypeScheme val (arg ': args) res
    TypeSchemeAll
        :: (KnownSymbol text, KnownNat uniq)
           -- Here we require the user to manually provide the unique of a type variable.
           -- That's nothing but silly, but I do not see what else we can do with the current design.
        => Proxy '(text, uniq, kind)
        -> TypeScheme val args res
        -> TypeScheme val args res
