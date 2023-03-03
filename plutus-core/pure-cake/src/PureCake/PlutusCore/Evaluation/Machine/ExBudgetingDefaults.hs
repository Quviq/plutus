-- editorconfig-checker-disable-file
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE EmptyCase   #-}

module PureCake.PlutusCore.Evaluation.Machine.ExBudgetingDefaults
    ( defaultCekParameters
    )

where

import PlutusCore.DataFilePaths qualified as DFP

import PureCake.PlutusCore.Evaluation.Machine.MachineParameters

import PureCake.UntypedPlutusCore.Evaluation.Machine.Cek.CekMachineCosts
import PureCake.UntypedPlutusCore.Evaluation.Machine.Cek.Internal

import Data.Aeson.THReader

-- | Default costs for CEK machine instructions.
defaultCekMachineCosts :: CekMachineCosts
defaultCekMachineCosts =
  $$(readJSONFromFile DFP.cekMachineCostsFile)

defaultCekParameters :: MachineParameters CekMachineCosts CekValue
defaultCekParameters = mkMachineParameters defaultCekMachineCosts
