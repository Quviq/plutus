-- editorconfig-checker-disable-file
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module PureCake.UntypedPlutusCore.Evaluation.Machine.Cek.ExBudgetMode
    ( ExBudgetMode (..)
    , RestrictingSt (..)
    , Hashable
    , restricting
    )
where

import PlutusPrelude

import PureCake.UntypedPlutusCore.Evaluation.Machine.Cek.Internal

import PureCake.PlutusCore.Evaluation.Machine.ExBudget
import PureCake.PlutusCore.Evaluation.Machine.Exception
import PureCake.PlutusCore.Evaluation.Machine.ExMemory (ExCPU (..), ExMemory (..))

import Control.Lens (ifoldMap)
import Control.Monad.Except
import Data.Hashable (Hashable)
import Data.HashMap.Monoidal as HashMap
import Data.List (intersperse)
import Data.Map.Strict qualified as Map
import Data.Primitive.PrimArray
import Data.SatInt
import Data.Semigroup.Generic
import Data.STRef

newtype RestrictingSt = RestrictingSt ExRestrictingBudget
    deriving stock (Eq, Show)
    deriving newtype (Semigroup, Monoid, NFData)

-- | For execution, to avoid overruns.
restricting :: ExRestrictingBudget -> ExBudgetMode RestrictingSt
restricting (ExRestrictingBudget initB@(ExBudget cpuInit memInit)) = ExBudgetMode $ do
    -- We keep the counters in a PrimArray. This is better than an STRef since it stores its contents unboxed.
    --
    -- If we don't specify the element type then GHC has difficulty inferring it, but it's
    -- annoying to specify the monad, since it refers to the 's' which is not in scope.
    ref <- newPrimArray @_ @SatInt 2
    let
        cpuIx = 0
        memIx = 1
        readCpu = coerce @_ @ExCPU <$> readPrimArray ref cpuIx
        writeCpu cpu = writePrimArray ref cpuIx $ coerce cpu
        readMem = coerce @_ @ExMemory <$> readPrimArray ref memIx
        writeMem mem = writePrimArray ref memIx $ coerce mem

    writeCpu cpuInit
    writeMem memInit
    let
        spend _ (ExBudget cpuToSpend memToSpend) = do
            cpuLeft <- CekM readCpu
            memLeft <- CekM readMem
            let cpuLeft' = cpuLeft - cpuToSpend
            let memLeft' = memLeft - memToSpend
            -- Note that even if we throw an out-of-budget error, we still need to record
            -- what the final state was.
            CekM $ writeCpu cpuLeft'
            CekM $ writeMem memLeft'
            when (cpuLeft' < 0 || memLeft' < 0) $ do
                let budgetLeft = ExBudget cpuLeft' memLeft'
                throwingWithCause _EvaluationError
                    (UserEvaluationError . CekOutOfExError $ ExRestrictingBudget budgetLeft)
                    Nothing
        spender = CekBudgetSpender spend
        remaining = ExBudget <$> readCpu <*> readMem
        cumulative = do
            r <- remaining
            pure $ initB `minusExBudget` r
        final = RestrictingSt . ExRestrictingBudget <$> remaining
    pure $ ExBudgetInfo spender final cumulative
