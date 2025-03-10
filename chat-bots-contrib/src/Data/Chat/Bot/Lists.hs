-- | A Bot Behavior for managing lists
module Data.Chat.Bot.Lists
  ( listsBot,
    listsBotSerializer,
    ListsState (..),
    ListAction,
  )
where

--------------------------------------------------------------------------------

import Control.Applicative
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Attoparsec.ByteString.Char8
  ( isSpace,
  )
import Data.Attoparsec.Text
import Data.Chat.Bot
import Data.Chat.Bot.Serialization
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

--------------------------------------------------------------------------------

data ListItemAction = Insert Text | Modify Int Text | Remove Int

data ListAction = CreateList Text | ModifyList Text ListItemAction | DeleteList Text | ShowList Text

newtype ListsState = ListsState {getListsState :: Map Text (IntMap Text)}
  deriving newtype (FromJSON, ToJSON, Semigroup, Monoid)

listItemBot :: (Monad m) => Bot m (IntMap Text) ListItemAction Text
listItemBot = Bot $ \s -> \case
  Insert todo ->
    let k = freshKey s in pure ("Entry added", IntMap.insert k todo s)
  Modify k todo -> pure ("Entry updated", IntMap.insert k todo s)
  Remove k -> pure ("Entry deleted", IntMap.delete k s)

freshKey :: IntMap a -> Int
freshKey state = case IntMap.lookupMax state of
  Nothing -> 0
  Just (k, _) -> k + 1

prettyList :: Text -> IntMap Text -> Text
prettyList name list = name <> ":\n" <> foldr (\(i, x) acc -> T.pack (show i) <> ". " <> x <> "\n" <> acc) mempty (IntMap.toList list)

prettyListM :: Text -> Maybe (IntMap Text) -> Text
prettyListM name = \case
  Nothing -> "List '" <> name <> "' not found."
  Just l -> prettyList name l

listsBot :: (Monad m) => Bot m ListsState ListAction Text
listsBot = Bot $ \(ListsState s) -> \case
  CreateList name -> pure ("List Created", ListsState $ Map.insert name mempty s)
  ModifyList name action -> do
    let t = fromMaybe IntMap.empty $ Map.lookup name s
    t' <- fmap snd $ runBot listItemBot t action
    pure ("List Updated", ListsState $ Map.insert name t' s)
  DeleteList name -> pure ("List deleted", ListsState $ Map.delete name s)
  ShowList name -> pure (prettyListM name $ Map.lookup name s, ListsState $ s)

--------------------------------------------------------------------------------

listsBotSerializer :: TextSerializer Text ListAction
listsBotSerializer = Serializer parseListAction id

parseListAction :: Text -> Maybe ListAction
parseListAction = either (const Nothing) Just . parseOnly listActionParser

listActionParser :: Parser ListAction
listActionParser =
  parseCreateList
    <|> parseDeleteList
    <|> parseAddListItem
    <|> parseRemoveListItem
    <|> parseUpdateListItem
    <|> parseShowList
  where
    parseName = ("📝" <|> "list") *> skipSpace *> takeTill isSpace
    parseCreateList = do
      name <- parseName
      skipSpace
      void $ "➕" <|> "create"
      skipSpace
      pure (CreateList name)
    parseDeleteList = do
      name <- parseName
      skipSpace
      void $ "➖" <|> "delete"
      skipSpace
      pure (DeleteList name)
    parseShowList = do
      name <- parseName
      pure (ShowList name)
    parseAddListItem = do
      name <- parseName
      skipSpace
      void ("📝" <|> "add")
      skipSpace
      item <- takeText
      pure (ModifyList name (Insert item))
    parseRemoveListItem = do
      name <- parseName
      skipSpace
      void ("✔️" <|> "remove")
      skipSpace
      key <- decimal
      pure (ModifyList name (Remove key))
    parseUpdateListItem = do
      name <- parseName
      skipSpace
      void "update"
      skipSpace
      key <- decimal
      skipSpace
      item <- takeText
      pure (ModifyList name (Modify key item))
