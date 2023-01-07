{-# OPTIONS -fdefer-typed-holes -Wno-orphans #-}
{-# LANGUAGE RankNTypes #-}

module CofreeBot.Bot.Behaviors.Calculator.Language where

import CofreeBot.Utils
import Control.Applicative
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.RWS.Class
import Control.Monad.State
import Data.Attoparsec.Text as A
import Data.Bifunctor
import Data.Char
  ( isAlpha,
    isDigit,
  )
import Data.Functor
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

--------------------------------------------------------------------------------
-- Utils

infixOp :: Parser a -> Parser b -> Parser T.Text -> Parser (a, b)
infixOp p1 p2 pop =
  "("
    |*| p1
    |*| some space
    |*| pop
    |*| some space
    |*| p2
    |*| ")"
    <&> \(_ :& e1 :& _ :& _ :& _ :& e2 :& _) -> (e1, e2)

--------------------------------------------------------------------------------
-- Parsing types

type VarName = T.Text

data Expr
  = Var VarName
  | Val Int
  | Add Expr Expr
  | Mult Expr Expr
  | Neg Expr

data Statement
  = Let T.Text Expr
  | StdOut Expr
  deriving (Show)

type Program = NE.NonEmpty Statement

--------------------------------------------------------------------------------
-- Printer

instance Show Expr where
  showsPrec p = \case
    Var x -> shows $ T.unpack x
    Val n -> shows n
    x `Add` y ->
      showParen (p >= 6) $ (showsPrec 6 x) . (" + " ++) . (showsPrec 6 y)
    x `Mult` y ->
      showParen (p >= 7) $ (showsPrec 7 x) . (" * " ++) . (showsPrec 7 y)
    Neg x -> shows $ "- " <> show x

--------------------------------------------------------------------------------
-- Parser

varNameP :: Parser VarName
varNameP =
  fmap (uncurry T.cons) $ letter |*| A.takeWhile (liftA2 (||) isAlpha isDigit)

exprP :: Parser Expr
exprP =
  asum
    [ fmap (uncurry Add) $ (exprP `infixOp` exprP) $ "+",
      fmap (uncurry Mult) $ (exprP `infixOp` exprP) $ "*",
      Neg <$> ("-" *> exprP),
      fmap Val $ decimal,
      fmap Var $ varNameP
    ]

statementP :: Parser Statement
statementP =
  asum
    [ varNameP
        |*| some space
        |*| ":="
        |*| some space
        |*| exprP
        <&> \(var :& _ :& _ :& _ :& expr) -> Let var expr,
      StdOut <$> exprP
    ]

programP :: Parser Program
programP =
  statementP
    |*| ([] <$ endOfInput <|> endOfLine *> statementP `sepBy` endOfLine)
    <&> uncurry (NE.:|)

-- $> import Data.Attoparsec.Text

-- $> import CofreeBot.Plugins.Calculator.Language

-- $> parseOnly exprP "((11 + x1) + 13)"

-- $> parseOnly programP "x := ((11 + 12) + 13)\nx + 1"

data ParseError = ParseError
  { parseInput :: T.Text,
    parseError :: T.Text
  }

parseStatement :: T.Text -> Either ParseError Statement
parseStatement txt = first (ParseError txt . T.pack) $ parseOnly statementP txt

parseProgram :: T.Text -> Either ParseError Program
parseProgram txt = first (ParseError txt . T.pack) $ parseOnly programP txt

--------------------------------------------------------------------------------
-- Evaluator

data CalcResp = Log Expr Int | Ack

data CalcError = LookupError T.Text
  deriving (Show)

type CalcState = Map.Map T.Text Int

-- | Evaluate an expression in our arithmetic language
eval :: Expr -> ExceptT CalcError (State CalcState) Int
eval = \case
  Var var ->
    maybe (throwError $ LookupError var) pure =<< gets (Map.lookup var)
  Val n -> pure n
  Add x y -> liftA2 (+) (eval x) (eval y)
  Mult x y -> liftA2 (*) (eval x) (eval y)
  Neg x -> fmap negate (eval x)

-- | Interpret a language statement into response.
interpretStatement :: Statement -> ExceptT CalcError (State CalcState) CalcResp
interpretStatement = \case
  Let bndr expr -> do
    val <- eval expr
    modify (Map.insert bndr val)
    pure Ack
  StdOut expr -> do
    val <- eval expr
    pure $ Log expr val

-- | Execute the calculator with a given statement and state
execCalculator :: Statement -> CalcState -> (CalcError \/ CalcResp, CalcState)
execCalculator statement calcState =
  flip runState calcState $ runExceptT $ interpretStatement statement
