{-# OPTIONS -fdefer-typed-holes -Wno-orphans #-}
module CofreeBot.Bot.Behaviors.Calculator.Language where

import           CofreeBot.Utils
import           Control.Applicative
import           Control.Monad.Error.Class
import           Control.Monad.Except
import           Control.Monad.RWS.Class
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Attoparsec.Text          as A
import           Data.Bifunctor
import           Data.Char                      ( isAlpha
                                                , isDigit
                                                )
import           Data.Coerce
import           Data.Foldable                  ( Foldable(fold)
                                                , asum
                                                )
import           Data.Functor
import qualified Data.List.NonEmpty            as NE
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

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
--------------------------------------------------------------------------------

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
  deriving Show

type Program = NE.NonEmpty Statement

--------------------------------------------------------------------------------
-- Printer
--------------------------------------------------------------------------------

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
--------------------------------------------------------------------------------

varNameP :: Parser VarName
varNameP =
  fmap (uncurry T.cons) $ letter |*| A.takeWhile (liftA2 (||) isAlpha isDigit)

exprP :: Parser Expr
exprP = asum
  [ fmap (uncurry Add) $ (exprP `infixOp` exprP) $ "+"
  , fmap (uncurry Mult) $ (exprP `infixOp` exprP) $ "*"
  , Neg <$> ("-" *> exprP)
  , fmap Val $ decimal
  , fmap Var $ varNameP
  ]

statementP :: Parser Statement
statementP = asum
  [ varNameP
  |*| some space
  |*| ":="
  |*| some space
  |*| exprP
  <&> \(var :& _ :& _ :& _ :& expr) -> Let var expr
  , StdOut <$> exprP
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
  { parseInput :: T.Text
  , parseError :: T.Text
  }

parseProgram :: T.Text -> Either ParseError Program
parseProgram txt = first (ParseError txt . T.pack) $ parseOnly programP txt

--------------------------------------------------------------------------------
-- Evaluation types
--------------------------------------------------------------------------------

data CalcError = LookupError T.Text
  deriving Show

type CalcState = Map.Map T.Text Int

data CalcResp = Log Expr Int

--------------------------------------------------------------------------------
-- Evaluator
--------------------------------------------------------------------------------

type CalculatorM
  = Transformers '[WriterT [CalcResp] , ExceptT CalcError , StateT CalcState] IO

-- | Evaluate an expression in our arithmetic language
eval :: Expr -> CalculatorM Int
eval = \case
  Var bndr -> do
    s <- get
    maybe (throwError $ LookupError bndr) pure $ Map.lookup bndr s
  Val i    -> pure i
  Add  x y -> liftA2 (+) (eval x) (eval y)
  Mult x y -> liftA2 (*) (eval x) (eval y)
  Neg x    -> fmap negate $ eval x

-- | Interpret a language statement into response.
interpretStatement :: Statement -> CalculatorM ()
interpretStatement = \case
  Let bndr expr -> do
    val <- eval expr
    modify (Map.insert bndr val)
  StdOut expr -> do
    val <- eval expr
    tell [Log expr val]

interpretProgram
  :: Program -> CalcState -> IO (Either CalcError [CalcResp], CalcState)
interpretProgram =
  (fmap . fmap . first . fmap) (snd @())
    . coerce
    . fmap fold
    . traverse interpretStatement
