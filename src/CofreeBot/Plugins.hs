{-# LANGUAGE EmptyCase #-}
module CofreeBot.Plugins where

import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TChan (TChan)
import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Text as T
import Network.Matrix.Client

newtype MatrixServer = MatrixServer T.Text

-- | An enumeration of all available plugins
data PluginLabel

-- | A response message to be sent to the matrix server
data MessageResponse = MessageResponse
  { room :: RoomID
  , msg :: Event
  , txId :: TxnID
  }

type Cache = M.Map T.Text JoinedRoomSync
type EventCache = TVar Cache
type ResponseChan = TChan MessageResponse

-- | This doesn't belong in this module..
data Config = Config
  { session :: ClientSession
  , eventCache :: EventCache
  , respChan :: ResponseChan
  }

-- | Super vague. I think we will want to change the cache to contain
-- a Map of Rooms to TChans.
fetchNewEvents :: Cache -> [Event]
fetchNewEvents = undefined

-- | Check incoming messaage for a plugin invocation
scanEvent :: Event -> Maybe PluginLabel
scanEvent = undefined

-- | Plugin subroutine with access to the EventCache TVar.
-- TODO: the EventCache TVar should be read only for plugins.
-- I little typeclass should do the trick? eg.,:
--
-- class Monad m => ReadCacheM m where
--   readCache :: m Cache
--
-- instance MonadIO m => ReadCacheM (ReaderT EventCache m) where
--   readCache = ask >>= \c -> liftIO $ readTVarIO c
invokePlugin
  :: PluginLabel
  -> ReaderT Config IO (Maybe MessageResponse)
invokePlugin = \case
