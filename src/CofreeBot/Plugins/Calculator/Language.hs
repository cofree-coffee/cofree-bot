{-# OPTIONS -fdefer-typed-holes -Wno-orphans #-}
{-# LANGUAGE PatternSynonyms #-}
module CofreeBot.Plugins.Calculator.Language where

import Control.Applicative
import Data.Attoparsec.Text as A
import Data.Bifunctor
import Data.Char (isAlpha, isDigit)
import Data.Foldable (asum)
import Data.Functor
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

data CalcError = LookupError T.Text
  deriving Show

type CalcState = Map.Map T.Text Int

data CalcResp = Log Expr Int | LogErr T.Text

type VarName = T.Text

data Expr = Var VarName | Val Int | Add Expr Expr | Mult Expr Expr | Neg Expr

data Statement = Let T.Text Expr | StdOut Expr | StdErr T.Text

type Program = NE.NonEmpty Statement

instance Show Expr where
  showsPrec p = \case
    Var x -> shows $ T.unpack x
    Val n -> shows n
    x `Add` y -> showParen (p >= 6) $ (showsPrec 6 x) . (" + " ++) . (showsPrec 6 y)
    x `Mult` y -> showParen (p >= 7) $ (showsPrec 7 x) . (" * " ++) . (showsPrec 7 y)
    Neg x -> shows $ "- " <> show x

(|*|) :: Applicative f => f a -> f b -> f (a, b)
(|*|) = liftA2 (,)

infixr |*|

pattern (:&) :: a -> b -> (a, b)
pattern a :& b = (a, b)

{-# COMPLETE (:&) #-}

infixr :&

infixOp :: Parser a -> Parser b -> Parser T.Text -> Parser (a, b)
infixOp p1 p2 pop =
  "(" |*| p1 |*| some space |*| pop |*| some space |*| p2 |*| ")" <&>
    \(_oparen :& e1 :& _space :& _plus :& _morespace :& e2 :& _cparen) -> (e1, e2)

parseVarName :: Parser VarName
parseVarName = fmap (\(l, ls) -> l `T.cons` ls) $ letter |*| A.takeWhile (liftA2 (||) isAlpha isDigit)

parseExpr :: Parser Expr
parseExpr = asum
  [ fmap (uncurry Add) $ (parseExpr `infixOp` parseExpr) $ "+"
  , fmap (uncurry Mult) $ (parseExpr `infixOp` parseExpr) $ "*"
  , Neg <$> ("-" *> parseExpr)
  , fmap Val $ decimal
  , fmap Var $ parseVarName
  ]

-- $> import Data.Attoparsec.Text

-- $> parseOnly parseExpr "((11 + x1) + 13)"

parseStmt :: Parser Statement
parseStmt = asum
  [ parseVarName |*| some space |*| ":=" |*| some space |*| parseExpr
    <&> \(var :& _ :& _ :& _ :& expr) -> Let var expr
  , StdOut <$> parseExpr
  ]

parseProgram :: Parser Program
parseProgram =
  parseStmt |*| (([] <$ endOfInput) <|> endOfLine *> parseStmt `sepBy` endOfLine)
  <&> uncurry (NE.:|)

-- $> parseOnly parseProgram "x := ((11 + 12) + 13)\nx + 1"

data ParseError = ParseError String

printTxt :: Either CalcError [CalcResp] -> [T.Text]
printTxt = \case
  Left err -> pure $ T.pack $ show err
  Right resps -> resps <&> \case
    Log expr n -> T.pack $ show expr <> " = " <> show n
    LogErr err -> err

parseTxt :: T.Text -> Either ParseError Program
parseTxt = first ParseError . parseOnly parseProgram
