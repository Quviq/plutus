{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE GADTs #-}

module PureCake.TestHarness where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.Text qualified as Text

import PlutusCore.Default qualified as PLC
import PlutusCore.Evaluation.Machine.ExBudget qualified as PLC
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults qualified as PLC
import PlutusCore.Evaluation.Machine.ExMemory qualified as PLC
import PlutusCore.Quote
import PlutusCore.TypeCheck qualified as PLC
import UntypedPlutusCore.Core qualified as PLC
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as PLC

import PureCake.Implementation qualified as Cake
import PureCake.ToPureCake

import Test.QuickCheck

import PlutusIR.Generators.QuickCheck.GenerateTerms
import PlutusIR.Generators.QuickCheck.ShrinkTerms

import PlutusCore.Compiler qualified as PLC
import PlutusIR.Compiler
import UntypedPlutusCore.DeBruijn qualified as PLC

noEmitter :: Cake.EmitterMode
noEmitter _ = pure $ Cake.CekEmitterInfo (\_ -> pure ()) (pure mempty)

logEmitter :: Cake.EmitterMode
logEmitter = error "TODO"

runPLC :: PLC.Term PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun ()
       -> ( Maybe Cake.Term
          , Cake.ExRestrictingBudget
          , [String] )
runPLC tm =
  let (e, c, l) = PLC.runCekDeBruijn PLC.defaultCekParameters
                                     (PLC.restricting testBudget)
                                     PLC.noEmitter
                                     tm
  in ( either (const Nothing) (Just . termToCake) e
     , restrictingStToCake c
     , Text.unpack <$> l
     )

noUnicode :: Cake.Term -> Cake.Term
noUnicode tm = case tm of
  Cake.Var{}           -> tm
  Cake.Builtin{}       -> tm
  Cake.Error{}         -> tm
  Cake.LamAbs name t -> Cake.LamAbs name (noUnicode t)
  Cake.Apply s t     -> Cake.Apply (noUnicode s) (noUnicode t)
  Cake.Force t       -> Cake.Force $ noUnicode t
  Cake.Delay t       -> Cake.Delay $ noUnicode t
  Cake.Constant c    -> Cake.Constant $ noUnicodeConst c

noUnicodeConst :: Cake.Const -> Cake.Const
noUnicodeConst c = case c of
  Cake.ConstString s -> Cake.ConstString $ filter (\ c -> c >= toEnum 31 && c < toEnum 128) s
  Cake.ConstPair a b -> Cake.ConstPair (noUnicodeConst a) (noUnicodeConst b)
  Cake.ConstList xs  -> Cake.ConstList $ map noUnicodeConst xs
  _ -> c

runCake :: PLC.Term PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun ()
        -> IO (Maybe Cake.Term, Cake.ExRestrictingBudget, [String])
runCake = runCake' . termToCake

runCake' :: Cake.Term -> IO (Maybe Cake.Term, Cake.ExRestrictingBudget, [String])
runCake' = Cake.runCekDeBruijn (exRestrictingBudgetToCake testBudget)
                               noEmitter
compileTm tm =
  let etm' = runExcept $ do
        tcConfig <- PLC.getDefTypeCheckConfig noProvenance
        let pirCtx = toDefaultCompilationCtx tcConfig & set ccTypeCheckConfig Nothing
        ctm <- flip runReaderT pirCtx $ runQuoteT $ compileTerm tm
        ctm' <- flip runReaderT PLC.defaultCompilationOpts $ runQuoteT $ PLC.compileTerm ctm
        ctm'' <- PLC.deBruijnTerm @(Error _ _ _) ctm'
        pure $ () <$ ctm''
  in case etm' of
      Left _    -> error "Something wrong"
      Right tm' -> tm'

prop_run_PLC_Cake :: Property
prop_run_PLC_Cake = withMaxSuccess 10000 $
  forAllShrink genTypeAndTerm_ shrinkClosedTypedTerm $ \ (_, tm) ->
    let tm' = compileTm tm in
    ioProperty $ (runPLC tm' ===) <$> runCake tm'

-- TODO: no clue about these numbers
testBudget :: PLC.ExRestrictingBudget
testBudget = PLC.ExRestrictingBudget $ PLC.ExBudget (PLC.ExCPU 100000) (PLC.ExMemory 100000)

data TestCase = TestCase String Cake.Term (Maybe Cake.Term, Cake.ExBudget, [String])
  deriving stock (Show)

generateTestCases :: FilePath -> Int -> IO ()
generateTestCases file n = do
  terms <- map (noUnicode . termToCake . compileTm . snd) <$> generate (vectorOf n genTypeAndTerm_)
  let mkTestCase tm = TestCase (show tm) tm <$> runCake' tm
  tests <- mapM mkTestCase terms
  writeFile file $ unlines $ "testCases =" : [ "  " ++ c ++ " " ++ show t | (t, c) <- zip tests ("[" : repeat ",") ]
                                          ++ ["  ]"]

