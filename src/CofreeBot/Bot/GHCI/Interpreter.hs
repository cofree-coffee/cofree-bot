-- | Blindly copied from https://github.com/isovector/design-tools/blob/master/src/Ghci.hs
module CofreeBot.Bot.GHCI.Interpreter (runGhci) where

import Control.Lens (_head, (%~))
import Data.Bool (bool)
import Data.List (intercalate, isPrefixOf, groupBy)
import Data.Text qualified as T
import GHC.Unicode (isSpace)
import System.Process (readCreateProcess, shell)

runGhci :: T.Text -> IO [(String, Maybe String)]
runGhci str
  = fmap (interleave (lines $ T.unpack str) . responses)
  . readProcess' "ghci" []
  $ unlines [unlines $ fmap removeSilent $ lines $ T.unpack str]

removeSilent :: String -> String
removeSilent ('@':s) = s
removeSilent s = s

responses :: String -> [String]
responses 
  = fmap unlines
  . fmap (_head %~ removeManyTags)
  . groupBy (\_ a -> not $ isResponse a)
  . drop 1
  . dropWhile (not . isPrefixOf "Ok, ")
  . lines

removeTag :: String -> String
removeTag = drop 2 . dropWhile (/= '>')

removeManyTags :: String -> String
removeManyTags ts = bool ts (removeManyTags $ removeTag ts) $ isResponse ts

isResponse :: String -> Bool
isResponse ('*':_) = True
isResponse x = isPrefixOf "ghci>" x

-- | Zip input lines to GHCI with responses from GHCI. Correctly deals with
-- input like @let x = 5@ which doesn't give a response.
interleave ::  [String] -> [String] -> [(String, Maybe String)]
interleave as
  = filter (not . null . fst)
  . zipping
      isSilent
      (\a ->
         if isReallySilent a
               then ("", Nothing)
               else (a, Nothing))
      (\a b -> (a, Just $ initNonEmpty $ b))
      (fmap (dropWhile isSpace) as)

initNonEmpty :: [a] -> [a]
initNonEmpty [] = []
initNonEmpty a = init a

isSilent :: String -> Bool
isSilent str
  | isPrefixOf ":set "     str = True
  | isPrefixOf "let "      str = True
  | isPrefixOf "type "     str = True
  | isPrefixOf "import "   str = True
  | isPrefixOf "default (" str = True
  | otherwise = isReallySilent str

-- | Input that starts wih a @\@@ is considered really silent, and doesn't even
-- appear as prompted input.
isReallySilent :: String -> Bool
isReallySilent str
  | isPrefixOf "@" str = True
  | otherwise = False

-- | Probably the worst function I've ever written. I don't know what this
-- does. Sorry!
zipping :: (a -> Bool) -> (a -> c) -> (a -> b -> c) -> [a] -> [b] -> [c]
zipping _ _ _ [] _ = []
zipping _ _ _ _ [] = []
zipping p d f (a:as) bs | p a = d a   : zipping p d f as bs
zipping p d f (a:as) (b:bs)   = f a b : zipping p d f as bs

readProcess' :: String -> [String] -> String -> IO String
readProcess' cmd args = readCreateProcess $ shell $ intercalate " " (cmd : args) <> " 2>&1"
