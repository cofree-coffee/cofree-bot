{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Scripts
  ( Script (..),
    Interaction (..),
    mkScript,
  )
where

--------------------------------------------------------------------------------

import Control.Applicative (asum)
import Control.Monad (void)
import Data.Attoparsec.Text
import Data.Text (Text)
import Data.Text qualified as Text
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift)

--------------------------------------------------------------------------------

data Line
  = StartInput Text
  | StartOutput Text
  | Continue Text
  deriving (Show)

end :: Parser ()
end = asum [void (satisfy isEndOfLine), endOfInput]

line :: Parser Text
line = takeWhile1 (not . isEndOfLine) <* end

parseLines :: Parser [Line]
parseLines = do
  many1 $
    asum
      [ fmap StartInput (skipSpace *> void ">>>" *> line),
        fmap StartOutput (skipSpace *> void "<<<" *> line),
        fmap Continue (skipSpace *> line)
      ]

data Message
  = Input Text
  | Output Text
  deriving stock (Show)

following :: [Line] -> (Text, [Line])
following [] = ("", [])
following (Continue l : ls) =
  let (a, b) = following ls
   in ("\n" <> l <> a, b)
following ls = ("", ls)

aggregateLines :: [Line] -> [Message]
aggregateLines [] = []
aggregateLines (StartInput l0 : ls) =
  let (a, b) = following ls
   in Input (l0 <> a) : aggregateLines b
aggregateLines (StartOutput l0 : ls) =
  let (a, b) = following ls
   in Output (l0 <> a) : aggregateLines b
-- ignore lines before the first >>> or <<<
aggregateLines (Continue _ : ls) = aggregateLines ls

aggregateScript :: [Message] -> Script
aggregateScript history = Script $ reverse $ go history []
  where
    go :: [Message] -> [Interaction Text Text] -> [Interaction Text Text]
    go [] res = res
    go (Input x : xs) res = go xs ((Interaction x []) : res)
    go (Output _ : _) [] = error "Recieved an output without an input"
    go (Output o : xs) (Interaction i os : res) = go xs (Interaction i (o : os) : res)

data Interaction i o = Interaction {providedInput :: i, expectedOutput :: [o]}
  deriving stock (Show, Read, Eq, Ord, Lift)

newtype Script = Script [Interaction Text Text]
  deriving stock (Lift)
  deriving newtype (Show, Read, Eq, Ord)

parseScript :: Parser Script
parseScript = do
  fmap (aggregateScript . aggregateLines) parseLines

mkScript :: QuasiQuoter
mkScript =
  QuasiQuoter {quoteExp, quotePat, quoteType, quoteDec}
  where
    quotePat _ = error "'script' does not support quoting patterns"
    quoteType _ = error "'script' does not support quoting types"
    quoteDec _ = error "'script' does not support quoting declarations"
    quoteExp str = case parseOnly parseScript (Text.pack str) of
      Left err -> error err
      Right result -> [|result|]
