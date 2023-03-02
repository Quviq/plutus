-- editorconfig-checker-disable-file
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module PureCake.PlutusCore.Default.Builtins
  ( DefaultFun(..)
  ) where

import PlutusPrelude

import PlutusCore.Data
import PureCake.PlutusCore.Builtin
import PureCake.PlutusCore.Default.Universe
import PureCake.PlutusCore.Evaluation.Machine.BuiltinCostModel
import PureCake.PlutusCore.Evaluation.Machine.ExBudget
import PureCake.PlutusCore.Evaluation.Machine.ExMemory
import PureCake.PlutusCore.Evaluation.Result

import Codec.Serialise (serialise)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Ix
import Data.Text (Text)

data DefaultFun
    -- Integers
    = AddInteger
    | SubtractInteger
    | MultiplyInteger
    | DivideInteger
    | QuotientInteger
    | RemainderInteger
    | ModInteger
    | EqualsInteger
    | LessThanInteger
    | LessThanEqualsInteger
    -- Bytestrings
    | AppendByteString
    | ConsByteString
    | SliceByteString
    | LengthOfByteString
    | IndexByteString
    | EqualsByteString
    | LessThanByteString
    | LessThanEqualsByteString
    -- Strings
    | AppendString
    | EqualsString
    -- Data
    | ConstrData
    | MapData
    | ListData
    | IData
    | BData
    | UnConstrData
    | UnMapData
    | UnListData
    | UnIData
    | UnBData
    | EqualsData
    | SerialiseData
    deriving stock (Show, Eq, Ord, Enum, Bounded, Generic, Ix)
    deriving anyclass (NFData, Hashable)

instance ExMemoryUsage DefaultFun where
    memoryUsage _ = 1

-- | Turn a function into another function that returns 'EvaluationFailure' when its second argument
-- is 0 or calls the original function otherwise and wraps the result in 'EvaluationSuccess'.
-- Useful for correctly handling `div`, `mod`, etc.
nonZeroArg :: (Integer -> Integer -> Integer) -> Integer -> Integer -> EvaluationResult Integer
nonZeroArg _ _ 0 = EvaluationFailure
nonZeroArg f x y = EvaluationSuccess $ f x y

instance uni ~ DefaultUni => ToBuiltinMeaning uni DefaultFun where
    type CostingPart uni DefaultFun = BuiltinCostModel

    data BuiltinVersion DefaultFun = DefaultFunV1 | DefaultFunV2
        deriving stock (Enum, Bounded, Show)

    -- Integers
    toBuiltinMeaning
        :: forall val. HasMeaningIn uni val
        => BuiltinVersion DefaultFun -> DefaultFun -> BuiltinMeaning val BuiltinCostModel
    toBuiltinMeaning _ver AddInteger =
        makeBuiltinMeaning
            ((+) @Integer)
            (runCostingFunTwoArguments . paramAddInteger)
    toBuiltinMeaning _ver SubtractInteger =
        makeBuiltinMeaning
            ((-) @Integer)
            (runCostingFunTwoArguments . paramSubtractInteger)
    toBuiltinMeaning _ver MultiplyInteger =
        makeBuiltinMeaning
            ((*) @Integer)
            (runCostingFunTwoArguments . paramMultiplyInteger)
    toBuiltinMeaning _ver DivideInteger =
        makeBuiltinMeaning
            (nonZeroArg div)
            (runCostingFunTwoArguments . paramDivideInteger)
    toBuiltinMeaning _ver QuotientInteger =
        makeBuiltinMeaning
            (nonZeroArg quot)
            (runCostingFunTwoArguments . paramQuotientInteger)
    toBuiltinMeaning _ver RemainderInteger =
        makeBuiltinMeaning
            (nonZeroArg rem)
            (runCostingFunTwoArguments . paramRemainderInteger)
    toBuiltinMeaning _ver ModInteger =
        makeBuiltinMeaning
            (nonZeroArg mod)
            (runCostingFunTwoArguments . paramModInteger)
    toBuiltinMeaning _ver EqualsInteger =
        makeBuiltinMeaning
            ((==) @Integer)
            (runCostingFunTwoArguments . paramEqualsInteger)
    toBuiltinMeaning _ver LessThanInteger =
        makeBuiltinMeaning
            ((<) @Integer)
            (runCostingFunTwoArguments . paramLessThanInteger)
    toBuiltinMeaning _ver LessThanEqualsInteger =
        makeBuiltinMeaning
            ((<=) @Integer)
            (runCostingFunTwoArguments . paramLessThanEqualsInteger)
    -- Bytestrings
    toBuiltinMeaning _ver AppendByteString =
        makeBuiltinMeaning
            BS.append
            (runCostingFunTwoArguments . paramAppendByteString)
    toBuiltinMeaning ver ConsByteString =
        -- The costing function is the same for all versions of this builtin, but since the
        -- denotation of the builtin accepts constants of different types ('Integer' vs 'Word8'),
        -- the costing function needs to by polymorphic over the type of constant.
        let costingFun :: ExMemoryUsage a => BuiltinCostModel -> a -> BS.ByteString -> ExBudget
            costingFun = runCostingFunTwoArguments . paramConsByteString
        -- See Note [Versioned builtins]
        in case ver of
            DefaultFunV1 -> makeBuiltinMeaning
               @(Integer -> BS.ByteString -> BS.ByteString)
               (\n xs -> BS.cons (fromIntegral @Integer n) xs)
               costingFun
            -- For versions other (i.e. larger) than V1, the first input must be in range [0..255].
            -- See Note [How to add a built-in function: simple cases]
            _ -> makeBuiltinMeaning
              @(Word8 -> BS.ByteString -> BS.ByteString)
              BS.cons
              costingFun
    toBuiltinMeaning _ver SliceByteString =
        makeBuiltinMeaning
            (\start n xs -> BS.take n (BS.drop start xs))
            (runCostingFunThreeArguments . paramSliceByteString)
    toBuiltinMeaning _ver LengthOfByteString =
        makeBuiltinMeaning
            BS.length
            (runCostingFunOneArgument . paramLengthOfByteString)
    toBuiltinMeaning _ver IndexByteString =
        makeBuiltinMeaning
            (\xs n -> if n >= 0 && n < BS.length xs then EvaluationSuccess $ toInteger $ BS.index xs n else EvaluationFailure)
            -- TODO: fix the mess above with `indexMaybe` from `bytestring >= 0.11.0.0`.
            (runCostingFunTwoArguments . paramIndexByteString)
    toBuiltinMeaning _ver EqualsByteString =
        makeBuiltinMeaning
            ((==) @BS.ByteString)
            (runCostingFunTwoArguments . paramEqualsByteString)
    toBuiltinMeaning _ver LessThanByteString =
        makeBuiltinMeaning
            ((<) @BS.ByteString)
            (runCostingFunTwoArguments . paramLessThanByteString)
    toBuiltinMeaning _ver LessThanEqualsByteString =
        makeBuiltinMeaning
            ((<=) @BS.ByteString)
            (runCostingFunTwoArguments . paramLessThanEqualsByteString)
    -- Strings
    toBuiltinMeaning _ver AppendString =
        makeBuiltinMeaning
            ((<>) @Text)
            (runCostingFunTwoArguments . paramAppendString)
    toBuiltinMeaning _ver EqualsString =
        makeBuiltinMeaning
            ((==) @Text)
            (runCostingFunTwoArguments . paramEqualsString)
    toBuiltinMeaning _ver ConstrData =
        makeBuiltinMeaning
            Constr
            (runCostingFunTwoArguments . paramConstrData)
    toBuiltinMeaning _ver MapData =
        makeBuiltinMeaning
            Map
            (runCostingFunOneArgument . paramMapData)
    toBuiltinMeaning _ver ListData =
        makeBuiltinMeaning
            List
            (runCostingFunOneArgument . paramListData)
    toBuiltinMeaning _ver IData =
        makeBuiltinMeaning
            I
            (runCostingFunOneArgument . paramIData)
    toBuiltinMeaning _ver BData =
        makeBuiltinMeaning
            B
            (runCostingFunOneArgument . paramBData)
    toBuiltinMeaning _ver UnConstrData =
        makeBuiltinMeaning
            (\case
                Constr i ds -> EvaluationSuccess (i, ds)
                _           -> EvaluationFailure)
            (runCostingFunOneArgument . paramUnConstrData)
    toBuiltinMeaning _ver UnMapData =
        makeBuiltinMeaning
            (\case
                Map es -> EvaluationSuccess es
                _      -> EvaluationFailure)
            (runCostingFunOneArgument . paramUnMapData)
    toBuiltinMeaning _ver UnListData =
        makeBuiltinMeaning
            (\case
                List ds -> EvaluationSuccess ds
                _       -> EvaluationFailure)
            (runCostingFunOneArgument . paramUnListData)
    toBuiltinMeaning _ver UnIData =
        makeBuiltinMeaning
            (\case
                I i -> EvaluationSuccess i
                _   -> EvaluationFailure)
            (runCostingFunOneArgument . paramUnIData)
    toBuiltinMeaning _ver UnBData =
        makeBuiltinMeaning
            (\case
                B b -> EvaluationSuccess b
                _   -> EvaluationFailure)
            (runCostingFunOneArgument . paramUnBData)
    toBuiltinMeaning _ver EqualsData =
        makeBuiltinMeaning
            ((==) @Data)
            (runCostingFunTwoArguments . paramEqualsData)
    toBuiltinMeaning _ver SerialiseData =
        makeBuiltinMeaning
            (BSL.toStrict . serialise @Data)
            (runCostingFunOneArgument . paramSerialiseData)

instance Default (BuiltinVersion DefaultFun) where
    def = DefaultFunV2
