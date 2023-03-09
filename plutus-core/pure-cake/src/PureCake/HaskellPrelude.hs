module PureCake.HaskellPrelude
  ( raise
  , tryError
  , newArray
  , readArray
  , writeArray
  )
  where

import Control.Monad.Catch (throwM, catch, SomeException)
import Data.Primitive.PrimArray
import GHC.Prim

raise :: IO a
raise = throwM $ userError "Bad dog"

tryError :: IO a -> IO (Maybe a)
tryError m = fmap Just m `catch` \ (_ :: SomeException) -> pure Nothing

newArray :: Int -> IO (MutablePrimArray RealWorld Int)
newArray = newPrimArray

readArray :: MutablePrimArray RealWorld Int -> Int -> IO Int
readArray = readPrimArray

writeArray :: MutablePrimArray RealWorld Int -> Int -> Int -> IO ()
writeArray = writePrimArray
