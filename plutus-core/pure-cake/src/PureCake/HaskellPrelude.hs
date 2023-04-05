module PureCake.HaskellPrelude
  ( Bool(..)
  , Integer
  , String
  , Maybe(..)
  , IO
  , pure
  , len
  , maybe
  , error
  , ($), (.)
  , (||)
  , (==), (<), (>=)
  , (-), (+), (*)
  , index
  , raise
  , tryError
  , newArray
  , readArray
  , writeArray
  , eqString
  )
  where

import Control.Monad.Catch (SomeException, catch, throwM)
import Data.Primitive.PrimArray
import GHC.Prim

len :: [a] -> Integer
len = fromIntegral . length

index :: [a] -> Integer -> a
index xs i = xs !! fromIntegral i

raise :: IO a
raise = throwM $ userError "Bad dog"

tryError :: IO a -> IO (Maybe a)
tryError m = fmap Just m `catch` \ (_ :: SomeException) -> pure Nothing

newArray :: Integer -> IO (MutablePrimArray RealWorld Int)
newArray = newPrimArray . fromIntegral

readArray :: MutablePrimArray RealWorld Int -> Integer -> IO Integer
readArray arr i = fromIntegral <$> readPrimArray arr (fromIntegral i)

writeArray :: MutablePrimArray RealWorld Int -> Integer -> Integer -> IO ()
writeArray arr i v = writePrimArray arr (fromIntegral i) (fromIntegral v)

eqString :: String -> String -> Bool
eqString = (==)
