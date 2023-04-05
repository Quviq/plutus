
{-# OPTIONS_GHC -Wno-unused-matches #-}
module PureCake.PureCakeTestHarness where

import PureCake.HaskellPrelude
import PureCake.Implementation

type Output = (Maybe Term, ExRestrictingBudget, [String])

testCases :: [TestCase]
testCases =
  [ TestCase "5" (Constant (ConstInteger 5)) (Just (Constant (ConstInteger 5)), ExBudget 76900 99800, [])
  ]

-- PURECAKE START

data TestCase = TestCase String Term (Maybe Term, ExBudget, [String])

-- Equality on results ----------------------------------------------------

eqVar :: NamedDeBruijn -> NamedDeBruijn -> Bool
eqVar x y = case x of
  NamedDeBruijn s i -> case y of
    NamedDeBruijn s' j -> eqString s s' && i == j

eqTerm :: Term -> Term -> Bool
eqTerm t1 t2 = case t1 of
  Var x      -> case t2 of
    Var y       -> eqVar x y
    _           -> False
  LamAbs x t -> case t2 of
    LamAbs y s  -> eqVar x y && eqTerm t s
    _           -> False
  Apply s t  -> case t2 of
    Apply s' t' -> eqTerm s s' && eqTerm t t'
    _           -> False
  Force t    -> case t2 of
    Force s     -> eqTerm t s
    _           -> False
  Delay t    -> case t2 of
    Delay s     -> eqTerm t s
    _           -> False
  Constant c -> case t2 of
    Constant c' -> eqConst c c'
    _           -> False
  Builtin f  -> case t2 of
    Builtin g   -> eqBuiltin f g
    _           -> False
  Error      -> case t2 of
    Error       -> True
    _           -> False

eqConst :: Const -> Const -> Bool
eqConst c1 c2 = case c1 of
  ConstInteger n -> case c2 of
    ConstInteger m  -> n == m
    _               -> False
  ConstString s  -> case c2 of
    ConstString s'  -> eqString s s'
    _               -> False
  ConstBool b    -> case c2 of
    ConstBool b'    -> eqBool b b'
    _               -> False
  ConstUnit      -> case c2 of
    ConstUnit       -> True
    _               -> False
  ConstPair a b  -> case c2 of
    ConstPair a' b' -> eqConst a a' && eqConst b b'
    _               -> False
  ConstList xs   -> case c2 of
    ConstList ys    -> eqList eqConst xs ys
    _               -> False

eqBool :: Bool -> Bool -> Bool
eqBool a b = case a of
  True  -> b
  False -> not b

eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList eq xs ys = case xs of
  [] -> case ys of
    [] -> True
    _ -> False
  x : xs' -> case ys of
    y : ys' -> eq x y && eqList eq xs' ys'
    _ -> False

eqBuiltin :: DefaultFun -> DefaultFun -> Bool
eqBuiltin b1 b2 = case b1 of
  AddInteger -> case b2 of
    AddInteger -> True

eqMaybe :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
eqMaybe eq m m' = case m of
  Nothing -> case m' of
    Nothing -> True
    _ -> False
  Just x -> case m' of
    Just y -> eq x y
    _ -> False

eqBudget :: ExBudget -> ExBudget -> Bool
eqBudget b b' = case b of
  ExBudget cpu mem -> case b' of
    ExBudget cpu' mem' -> cpu == cpu' && mem == mem'

eqOutput :: Output -> Output -> Bool
eqOutput o1 o2 = case o1 of
  (t, b, out) -> case o2 of
    (t', b', out') -> eqMaybe eqTerm t t' && eqBudget b b' && eqList eqString out out'

-- Main driver ------------------------------------------------------------

testBudget :: ExRestrictingBudget
testBudget = ExBudget 100000 100000

noEmitter :: EmitterMode
noEmitter x = pure (CekEmitterInfo (\ ss -> pure ()) (pure []))

runCek :: Term -> IO Output
runCek t = runCekDeBruijn testBudget noEmitter t

runTest :: TestCase -> IO ()
runTest t = case t of
  TestCase msg input expect -> do
    actual <- runCek input
    if eqOutput expect actual
      then pure ()
      else putStrLn msg

main :: IO ()
main = do
  mapM_ runTest testCases

-- PURECAKE STOP
