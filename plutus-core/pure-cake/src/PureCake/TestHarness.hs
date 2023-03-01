{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

module PureCake.TestHarness where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.Text

import PlutusCore.DeBruijn.Internal qualified as PLC
import PlutusCore.Default.Builtins qualified as PLC
import PlutusCore.Default.Universe qualified as PLC
import PlutusCore.Evaluation.Machine.ExBudget qualified as PLC
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults qualified as PLC
import PlutusCore.Evaluation.Machine.ExMemory qualified as PLC
import PlutusCore.Quote
import PlutusCore.TypeCheck qualified as PLC
import UntypedPlutusCore.Core qualified as PLC
import UntypedPlutusCore.Evaluation.Machine.Cek.EmitterMode qualified as PLC
import UntypedPlutusCore.Evaluation.Machine.Cek.ExBudgetMode qualified as PLC
import UntypedPlutusCore.Evaluation.Machine.Cek.Internal qualified as PLC

import PureCake.PlutusCore.DeBruijn.Internal qualified as Cake
import PureCake.PlutusCore.Default.Builtins qualified as Cake
import PureCake.PlutusCore.Default.Universe qualified as Cake
import PureCake.PlutusCore.Evaluation.Machine.ExBudgetingDefaults qualified as Cake
import PureCake.UntypedPlutusCore.Core qualified as Cake
import PureCake.UntypedPlutusCore.Evaluation.Machine.Cek.ExBudgetMode qualified as Cake
import PureCake.UntypedPlutusCore.Evaluation.Machine.Cek.Internal qualified as Cake


import PureCake.ToPureCake

import Test.QuickCheck

import PlutusIR.Generators.QuickCheck.GenerateTerms
import PlutusIR.Generators.QuickCheck.ShrinkTerms

import PlutusCore.Compiler qualified as PLC
import PlutusIR.Compiler
import UntypedPlutusCore.DeBruijn qualified as PLC

noEmitter :: Cake.EmitterMode uni fun
noEmitter = Cake.EmitterMode $ \_ -> pure $ Cake.CekEmitterInfo (\_ -> pure ()) (pure mempty)

logEmitter :: Cake.EmitterMode uni fun
logEmitter = error "TODO"

runPLC :: PLC.Term PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun ()
       -> ( Either (Cake.CekEvaluationException Cake.NamedDeBruijn Cake.DefaultUni Cake.DefaultFun)
                   (Cake.Term Cake.NamedDeBruijn Cake.DefaultUni Cake.DefaultFun ())
          , Cake.RestrictingSt
          , [Text] )
runPLC tm =
  let (e, c, l) = PLC.runCekDeBruijn PLC.defaultCekParameters
                                     (PLC.restricting testBudget)
                                     PLC.logEmitter
                                     tm
  in ( either (Left . cekExceptionToCake) (Right . termToCake) e
     , restrictingStToCake c
     , l
     )

runCake :: PLC.Term PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun ()
        -> ( Either (Cake.CekEvaluationException Cake.NamedDeBruijn Cake.DefaultUni Cake.DefaultFun)
                    (Cake.Term Cake.NamedDeBruijn Cake.DefaultUni Cake.DefaultFun ())
           , Cake.RestrictingSt
           , [Text] )
runCake =
  Cake.runCekDeBruijn Cake.defaultCekParameters
                      (Cake.restricting $ exRestrictingBudgetToCake testBudget)
                      noEmitter
  . termToCake

prop_run_PLC_Cake :: Property
prop_run_PLC_Cake =
  forAllShrink genTypeAndTerm_ shrinkClosedTypedTerm $ \ (_, tm) ->
    let etm' = runExcept $ do
          tcConfig <- PLC.getDefTypeCheckConfig noProvenance
          let pirCtx = toDefaultCompilationCtx tcConfig & set ccTypeCheckConfig Nothing
          ctm <- flip runReaderT pirCtx $ runQuoteT $ compileTerm tm
          ctm' <- flip runReaderT PLC.defaultCompilationOpts $ runQuoteT $ PLC.compileTerm ctm
          ctm'' <- PLC.deBruijnTerm @(Error _ _ _) ctm'
          pure $ () <$ ctm''
    in case etm' of
        Left _    -> error "Something wrong"
        Right tm' -> runPLC tm' === runCake tm'

-- TODO: no clue about these numbers
testBudget :: PLC.ExRestrictingBudget
testBudget = PLC.ExRestrictingBudget $ PLC.ExBudget (PLC.ExCPU 100000) (PLC.ExMemory 100000)
