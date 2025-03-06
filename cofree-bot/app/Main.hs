{-# LANGUAGE NumDecimals #-}

module Main where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.Except
  ( ExceptT,
    runExceptT,
  )
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Chat.Bot
import Data.Chat.Bot.Calculator
import Data.Chat.Bot.CoinFlip
import Data.Chat.Bot.Context
import Data.Chat.Bot.GHCI
import Data.Chat.Bot.HKD
import Data.Chat.Bot.Hello
import Data.Chat.Bot.Jitsi
import Data.Chat.Bot.Magic8Ball
import Data.Chat.Bot.Serialization qualified as S
import Data.Chat.Bot.Updog
import Data.Chat.Server
import Data.Chat.Server.Matrix
import Data.Chat.Server.Repl
import Data.Foldable (fold)
import Data.Text (Text)
import GHC.Conc (threadDelay)
import GHC.Generics (Generic, Generically (..))
import GHC.IO.Handle (Handle)
import Network.Matrix.Client (ClientSession, login)
import Options
import Options.Applicative qualified as Opt
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.Process.Typed (Process, getStdout, withProcessWait_)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  xdgCache <- getUserCacheDir "cofree-bot"
  Opt.execParser parserInfo >>= \case
    LoginCmd cred -> do
      session <- login cred
      matrixMain session xdgCache
    TokenCmd clientSessionArgv -> do
      putStrLn "Loading Config"
      clientSessionEnv <- fromEnv
      clientSessionConfigFile <- fromConfig
      putStrLn "Starting Cofree-Bot"
      toClientSession (fold [clientSessionArgv, clientSessionEnv, clientSessionConfigFile]) >>= \case
        Just session -> matrixMain session xdgCache
        Nothing -> error "Invalid Client Session"
    CLI -> do
      putStrLn "Starting Cofree-Bot"
      cliMain xdgCache

--------------------------------------------------------------------------------

data CofreeBot p = CofreeBot
  { hello :: p () () Text,
    updog :: p () Updog Text,
    coinFlip :: p () () Bool,
    magic8Ball :: p () () Int,
    jitsi :: p () () Text,
    ghci :: p () Text Text,
    calclator :: p (SessionState CalcState) (SessionInput Statement) (SessionOutput (Either CalcError CalcResp))
  }
  deriving stock (Generic)
  deriving anyclass (SequenceBot, SequenceSer)

deriving instance FromJSON (CofreeBot StateF)

deriving instance ToJSON (CofreeBot StateF)

deriving via (Generically (CofreeBot StateF)) instance Semigroup (CofreeBot StateF)

deriving via (Generically (CofreeBot StateF)) instance Monoid (CofreeBot StateF)

bot' :: Process Handle Handle () -> CofreeBot (Bot IO)
bot' process =
  CofreeBot
    helloBot
    updogBot
    coinFlipBot
    magic8BallBot
    jitsiBot
    (ghciBot process)
    (sessionize mempty calculatorBot)

serializer' :: CofreeBot Contorted
serializer' =
  CofreeBot
    (Contort helloBotSerializer)
    (Contort updogSerializer)
    (Contort coinFlipSerializer)
    (Contort magic8BallSerializer)
    (Contort jitsiSerializer)
    (Contort ghciSerializer)
    (Contort $ sessionSerializer calculatorSerializer)

bot :: Process Handle Handle () -> Bot IO (CofreeBot StateF) Text Text
bot process = S.applySerializer (sequenceBot $ bot' process) (sequenceSer serializer')

--------------------------------------------------------------------------------

cliMain :: FilePath -> IO ()
cliMain xdgCache = withProcessWait_ ghciConfig $ \process -> do
  void $ threadDelay 1e6
  void $ hGetOutput (getStdout process)
  state <- readState xdgCache
  fixedBot <- flip (fixBotPersistent xdgCache) (fold state) $ bot process
  void $ loop $ annihilate repl fixedBot

--------------------------------------------------------------------------------

unsafeCrashInIO :: (Show e) => ExceptT e IO a -> IO a
unsafeCrashInIO = runExceptT >=> either (fail . show) pure

matrixMain :: ClientSession -> FilePath -> IO ()
matrixMain session xdgCache = withProcessWait_ ghciConfig $ \process -> do
  void $ threadDelay 1e6
  void $ hGetOutput (getStdout process)
  state <- readState xdgCache
  fixedBot <- flip (fixBotPersistent xdgCache) (fold state) $ embedTextBot $ hoistBot liftIO $ bot process
  unsafeCrashInIO $ loop $ annihilate (matrix session xdgCache) $ batch $ fixedBot
