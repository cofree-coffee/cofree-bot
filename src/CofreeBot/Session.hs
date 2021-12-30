module CofreeBot.Session where

import CofreeBot.Bot.Matrix
import CofreeBot.Bot.Hello
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Data.Foldable
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.Matrix.Client
import Network.Matrix.Client.Lens
import Text.Pretty.Simple

newtype MatrixServer = MatrixServer { getMatrixServer :: T.Text }

-- NOTE: This needs to get handled properly:
runListener :: ClientSession -> Maybe T.Text -> IO ()
runListener session since =
  void $ runExceptT $ do
    userId <- ExceptT $ getTokenOwner session
    filterId <- ExceptT $ createFilter session userId messageFilter
    syncPoll session (Just filterId) since (Just Online) $ \syncResult -> do
       let newSince :: T.Text
           newSince = syncResult ^. _srNextBatch

           roomsMap :: Map.Map T.Text JoinedRoomSync
           roomsMap = syncResult ^. _srRooms . _Just . _srrJoin . ifolded 

           roomEvents :: Map.Map T.Text [RoomEvent]
           roomEvents = roomsMap <&> view (_jrsTimeline . _tsEvents . _Just)
           
           events :: [(RoomID, Event)]
           events = Map.foldMapWithKey (\rid es -> fmap ((RoomID rid,) . view _reContent) es) roomEvents

       liftIO $ writeFile "/tmp/cofree-bot-since_file" (T.unpack newSince)
       pPrint roomEvents
       liftIO $ traverse_ (\e -> runMatrixBot session e helloMatrixBot ()) events
