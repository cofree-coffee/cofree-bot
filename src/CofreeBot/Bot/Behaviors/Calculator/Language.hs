{-# OPTIONS -fdefer-typed-holes -Wno-orphans #-}
{-# language RankNTypes #-}
module CofreeBot.Bot.Behaviors.Calculator.Language where

import           CofreeBot.Utils
import           Control.Applicative
import           Control.Monad.Error.Class
import           Control.Monad.Except
import           Control.Monad.RWS.Class
import           Control.Monad.State
import           Data.Attoparsec.Text          as A
import           Data.Bifunctor
import           Data.Char                      ( isAlpha
                                                , isDigit
                                                )
import           Data.Foldable
import           Data.Functor
import           Data.Functor.Identity
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

-- $> import Data.Attoparsec.Text

-- $> import CofreeBot.Plugins.Calculator.Language

-- $> parseOnly exprP "((11 + x1) + 13)"

-- $> parseOnly programP "x := ((11 + 12) + 13)\nx + 1"

data ParseError = ParseError
  { parseInput :: T.Text
  , parseError :: T.Text
  }
  deriving Show

parseProgram :: T.Text -> Either ParseError Statement
parseProgram txt = first (ParseError txt . T.pack) $ parseOnly ("calc:" *> skipSpace *> statementP) txt

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

newtype CalculatorM m a =
  CalculatorM {runCalculatorM :: CalcState -> m (Either CalcError a, CalcState) }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState CalcState
    , MonadError CalcError
    )
  via
    Transformers '[ExceptT CalcError , StateT CalcState] m

execCalc
  :: CalcState
  -> (forall m . Monad m => CalculatorM m a)
  -> (Either CalcError a, CalcState)
execCalc calcState calc =
      runIdentity
    . runCalculatorM calc
    $ calcState

-- | Evaluate an expression in our arithmetic language
eval :: Monad m => Expr -> CalculatorM m Int
eval = \case
  Var bndr -> do
    s <- get
    maybe (throwError $ LookupError bndr) pure $ Map.lookup bndr s
  Val i    -> pure i
  Add  x y -> liftA2 (+) (eval x) (eval y)
  Mult x y -> liftA2 (*) (eval x) (eval y)
  Neg x    -> fmap negate $ eval x

-- | Interpret a language statement into response.
interpretStatement :: Monad m => Statement -> CalculatorM m (Maybe CalcResp)
interpretStatement = \case
  Let bndr expr -> do
    val <- eval expr
    modify (Map.insert bndr val)
    pure Nothing
  StdOut expr -> do
    val <- eval expr
    pure $ Just $ Log expr val

interpretProgram
  :: Statement -> CalcState -> (Either CalcError (Maybe CalcResp), CalcState)
interpretProgram program calcState =
  execCalc calcState $ interpretStatement program
