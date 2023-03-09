module PureCake.HaskellPrelude (raise, tryError) where

import Control.Monad.Catch (throwM, catch, SomeException)

raise :: IO a
raise = throwM $ userError "Bad dog"

tryError :: IO a -> IO (Maybe a)
tryError m = fmap Just m `catch` \ (_ :: SomeException) -> pure Nothing
