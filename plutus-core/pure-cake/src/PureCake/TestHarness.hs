
module PureCake.TestHarness where

import Data.Text

import PlutusCore.DeBruijn.Internal qualified as PLC
import PlutusCore.Default.Builtins qualified as PLC
import PlutusCore.Default.Universe qualified as PLC
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults qualified as PLC
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

noEmitter :: Cake.EmitterMode uni fun
noEmitter = Cake.EmitterMode $ \_ -> pure $ Cake.CekEmitterInfo (\_ -> pure ()) (pure mempty)

logEmitter :: Cake.EmitterMode uni fun
logEmitter = error "TODO"

runPLC :: PLC.Term PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun ()
       -> (Either (PLC.CekEvaluationException PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun)
                  (PLC.Term PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun ())
          , PLC.CountingSt
          , [Text] )
runPLC =
  PLC.runCekDeBruijn PLC.defaultCekParameters
                     PLC.counting
                     PLC.logEmitter

runCake :: PLC.Term PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun ()
        -> ( Either (Cake.CekEvaluationException Cake.NamedDeBruijn Cake.DefaultUni Cake.DefaultFun)
                   (Cake.Term Cake.NamedDeBruijn Cake.DefaultUni Cake.DefaultFun ())
           , Cake.CountingSt
           , [Text] )
runCake =
  Cake.runCekDeBruijn Cake.defaultCekParameters
                      Cake.counting
                      noEmitter
  . termToCake


