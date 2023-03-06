-- | This module defines a common type various evaluation machine use to return their results.

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UndecidableInstances  #-}

module PureCake.PlutusCore.Evaluation.Result
    ( AsEvaluationFailure (..)
    , _EvaluationFailureVia
    , EvaluationResult (..)
    ) where

import PlutusPrelude

import Control.Lens
import Control.Monad.Except

-- Note that we can't just use 'makeClassyPrisms' for 'EvaluationResult' as that would generate
-- @_EvaluationSuccess@ as well as @_EvaluationFailure@, which would no longer be prismatic error
-- handling: it'd constrain the monad, not the error in it.
--
-- We could define
--
--     data EvaluationFailure = EvaluationFailure
--
-- just for the purpose of using 'makeClassyPrisms' over it, but then it's clearer to write out
-- the class definition manually than to hide it behind a TH function called over a redundant
-- data type name-clashing with the useful 'EvaluationResult'.
-- | A class for viewing errors as evaluation failures (in the sense of Plutus).
class AsEvaluationFailure err where
  _EvaluationFailure :: Prism' err ()

-- | Construct a 'Prism' focusing on the @*EvaluationFailure@ part of @err@ by taking
-- that @*EvaluationFailure@ and
--
-- 1. returning it for the setter part of the prism
-- 2. checking the error for equality with @*EvaluationFailure@ for the opposite direction.
_EvaluationFailureVia :: Eq err => err -> Prism' err ()
_EvaluationFailureVia failure = prism (const failure) $ \a -> when (a /= failure) $ Left a

-- | The parameterized type of results various evaluation engines return.
-- On the PLC side this becomes (via @makeKnown@) either a call to 'Error' or
-- a value of the PLC counterpart of type @a@.
data EvaluationResult a
    = EvaluationSuccess !a
    | EvaluationFailure
    deriving stock (Show, Eq, Generic, Functor, Foldable, Traversable)

instance MonadError () EvaluationResult where
    throwError () = EvaluationFailure

    catchError EvaluationFailure f = f ()
    catchError x                 _ = x

instance Applicative EvaluationResult where
    pure = EvaluationSuccess

    EvaluationSuccess f <*> a = fmap f a
    EvaluationFailure   <*> _ = EvaluationFailure

instance Monad EvaluationResult where
    EvaluationSuccess x >>= f = f x
    EvaluationFailure   >>= _ = EvaluationFailure
