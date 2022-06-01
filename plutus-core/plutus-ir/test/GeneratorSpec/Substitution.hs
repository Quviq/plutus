{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module GeneratorSpec.Substitution where

import PlutusCore.Generators.PIR

import Data.Map qualified as Map
import Data.Set qualified as Set

import PlutusCore.Name
import PlutusCore.Quote (runQuote)
import PlutusCore.Rename
import PlutusIR.Subst

import Data.Maybe
import Data.String

import Test.QuickCheck

-- * Tests for unification and substitution

-- | Check that unification does the right thing.
prop_unify :: Property
prop_unify =
  forAllDoc "n"   arbitrary shrink         $ \ (NonNegative n) ->
  forAllDoc "m"   (choose (0, n)) shrink   $ \ m ->
  letCE "xs" (take n allTheVarsCalledX)    $ \ xs ->
  forAllDoc "ks"
    (vectorOf n arbitrary)
    (filter ((== n) . length) . shrink)    $ \ ks ->
  letCE "ctx" (Map.fromList                $ zip xs ks) $ \ ctx ->
  forAllDoc "ty1"
    (genTypeWithCtx ctx $ Star)
    (shrinkType ctx)                       $ \ ty1 ->
  forAllDoc "ty2"
    (genTypeWithCtx ctx $ Star)
    (shrinkType ctx)                       $ \ ty2 ->
  letCE "nty1" (normalizeTy ty1)           $ \ _ ->
  letCE "nty2" (normalizeTy ty2)           $ \ _ ->
  letCE "res" (unifyType ctx (Set.fromList $ take m xs) Map.empty ty1 ty2) $ \ res ->
  isJust res ==>
  let sub = fromJust res
      checkSub (x, ty) = letCE "x,ty" (x, ty)    $ \ _ ->
                         letCE "k" (ctx Map.! x) $ \ k -> checkKind ctx ty k
  in
  letCE "sty1" (substType sub ty1) $ \ sty1 ->
  letCE "sty2" (substType sub ty2) $ \ sty2 ->
  letCE "nsty1" (normalizeTy sty1) $ \ nsty1 ->
  letCE "nsty2" (normalizeTy sty2) $ \ nsty2 ->
  tabulate "sizes" [show $ min (Set.size $ ftvTy ty1) (Set.size $ ftvTy ty2)] $
  foldr (.&&.) (property $ nsty1 == nsty2) (map checkSub (Map.toList sub))
  where
    allTheVarsCalledX = [ TyName $ Name (fromString $ "x" ++ show i) (toEnum i) | i <- [1..] ]

-- | Check that a type unifies with a renaming of itself
prop_unifyRename :: Property
prop_unifyRename =
  forAllDoc "_, ty" genKindAndType (shrinkKindAndType mempty) $ \ (_, ty) ->
  letCE "rename ty" (runQuote $ rename ty) $ \ rnty ->
  isJust $ unifyType mempty mempty mempty ty rnty

-- | Check that substitution gets rid of all the right variables
prop_substType :: Property
prop_substType =
  -- No shrinking because every nested shrink makes properties
  -- harder to shrink and context minimality doesn't help readability very much.
  forAllDoc "ctx" genCtx (const []) $ \ ctx ->
  forAllDoc "ty" (genTypeWithCtx ctx Star) (shrinkType ctx) $ \ ty ->
  forAllDoc "sub" (genSubst ctx) (shrinkSubst ctx) $ \ sub ->
  letCE "res" (substType sub ty) $ \ res ->
  fvTypeR sub ty == ftvTy res && checkKind ctx res Star
