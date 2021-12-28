module CofreeBot.Plugins where

import qualified Data.Text as T

newtype MatrixServer = MatrixServer { getMatrixServer :: T.Text }

-- | An enumeration of all available plugins
data PluginLabel

-- | Plugin subroutine with access to the EventCache TVar.
-- TODO: the EventCache TVar should be read only for plugins.
-- I little typeclass should do the trick? eg.,:
--
-- class Monad m => ReadCacheM m where
--   readCache :: m Cache
--
-- instance MonadIO m => ReadCacheM (ReaderT EventCache m) where
--   readCache = ask >>= \c -> liftIO $ readTVarIO c
--invokePlugin
--  :: PluginLabel
--  -> ReaderT Config IO (Maybe MessageResponse)
--invokePlugin = \case
