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
import Data.ByteString.Hash qualified as Hash
import Data.ByteString.Lazy qualified as BSL
import Data.Ix
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import PureCake.Crypto (verifyEcdsaSecp256k1Signature, verifyEd25519Signature_V1, verifyEd25519Signature_V2,
                        verifySchnorrSecp256k1Signature)

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
    -- Cryptography and hashes
    | Sha2_256
    | Sha3_256
    | Blake2b_256
    | VerifyEd25519Signature  -- formerly verifySignature
    | VerifyEcdsaSecp256k1Signature
    | VerifySchnorrSecp256k1Signature
    -- Strings
    | AppendString
    | EqualsString
    | EncodeUtf8
    | DecodeUtf8
    -- Bool
    | IfThenElse
    -- Unit
    | ChooseUnit
    -- Tracing
    | Trace
    -- Pairs
    | FstPair
    | SndPair
    -- Lists
    | ChooseList
    | MkCons
    | HeadList
    | TailList
    | NullList
    -- Data
    -- See Note [Pattern matching on built-in types].
    -- It is convenient to have a "choosing" function for a data type that has more than two
    -- constructors to get pattern matching over it and we may end up having multiple such data
    -- types, hence we include the name of the data type as a suffix.
    | ChooseData
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
    -- Misc monomorphized constructors.
    -- We could simply replace those with constants, but we use built-in functions for consistency
    -- with monomorphic built-in types. Polymorphic built-in constructors are generally problematic,
    -- See note [Representable built-in functions over polymorphic built-in types].
    | MkPairData
    | MkNilData
    | MkNilPairData
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
    -- Cryptography and hashes
    toBuiltinMeaning _ver Sha2_256 =
        makeBuiltinMeaning
            Hash.sha2_256
            (runCostingFunOneArgument . paramSha2_256)
    toBuiltinMeaning _ver Sha3_256 =
        makeBuiltinMeaning
            Hash.sha3_256
            (runCostingFunOneArgument . paramSha3_256)
    toBuiltinMeaning _ver Blake2b_256 =
        makeBuiltinMeaning
            Hash.blake2b_256
            (runCostingFunOneArgument . paramBlake2b_256)
    toBuiltinMeaning ver VerifyEd25519Signature =
        let verifyEd25519Signature =
                case ver of
                  DefaultFunV1 -> verifyEd25519Signature_V1
                  _            -> verifyEd25519Signature_V2
        in makeBuiltinMeaning
           verifyEd25519Signature
           -- Benchmarks indicate that the two versions have very similar
           -- execution times, so it's safe to use the same costing function for
           -- both.
           (runCostingFunThreeArguments . paramVerifyEd25519Signature)
    {- Note [ECDSA secp256k1 signature verification].  An ECDSA signature
       consists of a pair of values (r,s), and for each value of r there are in
       fact two valid values of s, one effectively the negative of the other.
       The Bitcoin implementation that underlies `verifyEcdsaSecp256k1Signature`
       expects that the lower of the two possible values of the s component of
       the signature is used, returning `false` immediately if that's not the
       case.  It appears that this restriction is peculiar to Bitcoin, and ECDSA
       schemes in general don't require it.  Thus this function may be more
       restrictive than expected.  See

          https://github.com/bitcoin/bips/blob/master/bip-0146.mediawiki#LOW_S

       and the implementation of secp256k1_ecdsa_verify in

          https://github.com/bitcoin-core/secp256k1.
     -}
    toBuiltinMeaning _ver VerifyEcdsaSecp256k1Signature =
        makeBuiltinMeaning
            verifyEcdsaSecp256k1Signature
            (runCostingFunThreeArguments . paramVerifyEcdsaSecp256k1Signature)
    toBuiltinMeaning _ver VerifySchnorrSecp256k1Signature =
        makeBuiltinMeaning
            verifySchnorrSecp256k1Signature
            (runCostingFunThreeArguments . paramVerifySchnorrSecp256k1Signature)
    -- Strings
    toBuiltinMeaning _ver AppendString =
        makeBuiltinMeaning
            ((<>) @Text)
            (runCostingFunTwoArguments . paramAppendString)
    toBuiltinMeaning _ver EqualsString =
        makeBuiltinMeaning
            ((==) @Text)
            (runCostingFunTwoArguments . paramEqualsString)
    toBuiltinMeaning _ver EncodeUtf8 =
        makeBuiltinMeaning
            encodeUtf8
            (runCostingFunOneArgument . paramEncodeUtf8)
    toBuiltinMeaning _ver DecodeUtf8 =
        makeBuiltinMeaning
            (reoption @_ @EvaluationResult . decodeUtf8')
            (runCostingFunOneArgument . paramDecodeUtf8)
    -- Bool
    toBuiltinMeaning _ver IfThenElse =
        makeBuiltinMeaning
            (\b x y -> if b then x else y)
            (runCostingFunThreeArguments . paramIfThenElse)
    -- Unit
    toBuiltinMeaning _ver ChooseUnit =
        makeBuiltinMeaning
            (\() a -> a)
            (runCostingFunTwoArguments . paramChooseUnit)
    -- Tracing
    toBuiltinMeaning _ver Trace =
        makeBuiltinMeaning
            (\text a -> a <$ emit text)
            (runCostingFunTwoArguments . paramTrace)
    -- Pairs
    toBuiltinMeaning _ver FstPair =
        makeBuiltinMeaning
            fstPlc
            (runCostingFunOneArgument . paramFstPair)
        where
          fstPlc :: SomeConstant uni (a, b) -> EvaluationResult (Opaque val a)
          fstPlc (SomeConstant (Some (ValueOf uniPairAB xy))) = do
              DefaultUniPair uniA _ <- pure uniPairAB
              pure . fromValueOf uniA $ fst xy
          {-# INLINE fstPlc #-}
    toBuiltinMeaning _ver SndPair =
        makeBuiltinMeaning
            sndPlc
            (runCostingFunOneArgument . paramSndPair)
        where
          sndPlc :: SomeConstant uni (a, b) -> EvaluationResult (Opaque val b)
          sndPlc (SomeConstant (Some (ValueOf uniPairAB xy))) = do
              DefaultUniPair _ uniB <- pure uniPairAB
              pure . fromValueOf uniB $ snd xy
          {-# INLINE sndPlc #-}
    -- Lists
    toBuiltinMeaning _ver ChooseList =
        makeBuiltinMeaning
            choosePlc
            (runCostingFunThreeArguments . paramChooseList)
        where
          choosePlc :: SomeConstant uni [a] -> b -> b -> EvaluationResult b
          choosePlc (SomeConstant (Some (ValueOf uniListA xs))) a b = do
            DefaultUniList _ <- pure uniListA
            pure $ case xs of
                []    -> a
                _ : _ -> b
          {-# INLINE choosePlc #-}
    toBuiltinMeaning _ver MkCons =
        makeBuiltinMeaning
            consPlc
            (runCostingFunTwoArguments . paramMkCons)
        where
          consPlc
              :: SomeConstant uni a -> SomeConstant uni [a] -> EvaluationResult (Opaque val [a])
          consPlc
            (SomeConstant (Some (ValueOf uniA x)))
            (SomeConstant (Some (ValueOf uniListA xs))) = do
                DefaultUniList uniA' <- pure uniListA
                -- Checking that the type of the constant is the same as the type of the elements
                -- of the unlifted list. Note that there's no way we could enforce this statically
                -- since in UPLC one can create an ill-typed program that attempts to prepend
                -- a value of the wrong type to a list.
                -- Should that rather give us an 'UnliftingError'? For that we need
                -- https://github.com/input-output-hk/plutus/pull/3035
                Just Refl <- pure $ uniA `geq` uniA'
                pure . fromValueOf uniListA $ x : xs
          {-# INLINE consPlc #-}
    toBuiltinMeaning _ver HeadList =
        makeBuiltinMeaning
            headPlc
            (runCostingFunOneArgument . paramHeadList)
        where
          headPlc :: SomeConstant uni [a] -> EvaluationResult (Opaque val a)
          headPlc (SomeConstant (Some (ValueOf uniListA xs))) = do
              DefaultUniList uniA <- pure uniListA
              x : _ <- pure xs
              pure $ fromValueOf uniA x
          {-# INLINE headPlc #-}
    toBuiltinMeaning _ver TailList =
        makeBuiltinMeaning
            tailPlc
            (runCostingFunOneArgument . paramTailList)
        where
          tailPlc :: SomeConstant uni [a] -> EvaluationResult (Opaque val [a])
          tailPlc (SomeConstant (Some (ValueOf uniListA xs))) = do
              DefaultUniList _ <- pure uniListA
              _ : xs' <- pure xs
              pure $ fromValueOf uniListA xs'
          {-# INLINE tailPlc #-}
    toBuiltinMeaning _ver NullList =
        makeBuiltinMeaning
            nullPlc
            (runCostingFunOneArgument . paramNullList)
        where
          nullPlc :: SomeConstant uni [a] -> EvaluationResult Bool
          nullPlc (SomeConstant (Some (ValueOf uniListA xs))) = do
              DefaultUniList _ <- pure uniListA
              pure $ null xs
          {-# INLINE nullPlc #-}

    -- Data
    toBuiltinMeaning _ver ChooseData =
        makeBuiltinMeaning
            (\d
              xConstr
              xMap xList xI xB ->
                  case d of
                    Constr {} -> xConstr
                    Map    {} -> xMap
                    List   {} -> xList
                    I      {} -> xI
                    B      {} -> xB)
            (runCostingFunSixArguments . paramChooseData)
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
    -- Misc constructors
    toBuiltinMeaning _ver MkPairData =
        makeBuiltinMeaning
            ((,) @Data @Data)
            (runCostingFunTwoArguments . paramMkPairData)
    toBuiltinMeaning _ver MkNilData =
        -- Nullary built-in functions don't work, so we need a unit argument.
        -- We don't really need this built-in function, see Note [Constants vs built-in functions],
        -- but we keep it around for historical reasons and convenience.
        makeBuiltinMeaning
            (\() -> [] @Data)
            (runCostingFunOneArgument . paramMkNilData)
    toBuiltinMeaning _ver MkNilPairData =
        -- Nullary built-in functions don't work, so we need a unit argument.
        -- We don't really need this built-in function, see Note [Constants vs built-in functions],
        -- but we keep it around for historical reasons and convenience.
        makeBuiltinMeaning
            (\() -> [] @(Data,Data))
            (runCostingFunOneArgument . paramMkNilPairData)
    -- See Note [Inlining meanings of builtins].
    {-# INLINE toBuiltinMeaning #-}

instance Default (BuiltinVersion DefaultFun) where
    def = DefaultFunV2
