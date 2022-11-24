{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Scripts
  ( Script (..),
    mkScript,
  )
where

--------------------------------------------------------------------------------

import Control.Applicative (asum)
import Control.Monad (void)
import Data.Attoparsec.Text
  ( Parser,
    endOfInput,
    isEndOfLine,
    many',
    many1,
    parseOnly,
    satisfy,
    skipSpace,
    takeWhile1,
  )
import Data.Text (Text)
import Data.Text qualified as Text
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift)

--------------------------------------------------------------------------------

newtype Script = Script [(Text, [Text])]
  deriving stock (Lift)
  deriving newtype (Show, Read, Eq, Ord)

end :: Parser ()
end = asum [void (satisfy isEndOfLine), endOfInput]

line :: Parser Text
line = takeWhile1 (not . isEndOfLine) <* end

inputOutputs :: Parser (Text, [Text])
inputOutputs = do
  skipSpace
  input <- ">>>" *> line
  outputs <- many' (skipSpace *> "<<<" *> line)
  pure (input, outputs)

parseScript :: Parser Script
parseScript = Script <$> many1 inputOutputs

mkScript :: QuasiQuoter
mkScript =
  QuasiQuoter {quoteExp, quotePat, quoteType, quoteDec}
  where
    quotePat _ = error "'script' does not support quoting patterns"
    quoteType _ = error "'script' does not support quoting types"
    quoteDec _ = error "'script' does not support quoting declarations"
    quoteExp str = case parseOnly parseScript (Text.pack str) of
      Left _err -> error $ str <> " is not a valid script"
      Right result -> [|result|]
