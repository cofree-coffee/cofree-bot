module OptionsParser where

import qualified Data.Text                     as T
import           Network.Matrix.Client
import qualified Options.Applicative           as Opt

---------------------
--- Login Command ---
---------------------

parseLogin :: Opt.Parser LoginCredentials
parseLogin =
  LoginCredentials
    <$> parseUsername
    <*> parsePassword
    <*> fmap getMatrixServer parseServer
    <*> parseDeviceId
    <*> parseInitialDeviceName

parseUsername :: Opt.Parser Username
parseUsername = Username <$> Opt.strOption
  (  Opt.long "username"
  <> Opt.metavar "STRING"
  <> Opt.help
       "The fully qualified user ID or just local part of the user ID, to log in."
  )

parsePassword :: Opt.Parser LoginSecret
parsePassword = Password <$> Opt.strOption
  (Opt.long "password" <> Opt.metavar "STRING" <> Opt.help
    "The user's password."
  )

parseDeviceId :: Opt.Parser (Maybe DeviceId)
parseDeviceId = Opt.optional $ DeviceId <$> Opt.strOption
  (  Opt.long "device_id"
  <> Opt.metavar "STRING"
  <> Opt.help
       "ID of the client device. If this does not correspond to a known client device, a new device will be created. The server will auto-generate a device_id if this is not specified."
  )

parseInitialDeviceName :: Opt.Parser (Maybe InitialDeviceDisplayName)
parseInitialDeviceName =
  Opt.optional $ InitialDeviceDisplayName <$> Opt.strOption
    (  Opt.long "initial_device_name"
    <> Opt.metavar "STRING"
    <> Opt.help
         "A display name to assign to the newly-created device. Ignored if device_id corresponds to a known device."
    )

---------------------
--- Token Command ---
---------------------

data TokenCredentials = TokenCredentials
  { matrixToken  :: MatrixToken
  , matrixServer :: MatrixServer
  }

newtype MatrixServer = MatrixServer { getMatrixServer :: T.Text }

parseTokenCredentials :: Opt.Parser TokenCredentials
parseTokenCredentials = TokenCredentials <$> parseToken <*> parseServer

parseToken :: Opt.Parser MatrixToken
parseToken = MatrixToken <$> Opt.strOption
  (Opt.long "auth_token" <> Opt.metavar "MATRIX_AUTH_TOKEN" <> Opt.help
    "Matrix authentication token"
  )

parseServer :: Opt.Parser MatrixServer
parseServer = fmap MatrixServer $ Opt.strOption
  (Opt.long "homeserver" <> Opt.metavar "MATRIX_HOMESERVER" <> Opt.help
    "Matrix Homeserver"
  )

-------------------
--- Main Parser ---
-------------------

data Command = LoginCmd LoginCredentials | TokenCmd TokenCredentials | CLI

mainParser :: Opt.Parser Command
mainParser = Opt.subparser
  (  Opt.command
      "gen-token"
      (Opt.info (fmap LoginCmd parseLogin)
                (Opt.progDesc "Generate a token from a username/password")
      )
  <> Opt.command
       "run"
       (Opt.info (fmap TokenCmd parseTokenCredentials)
                 (Opt.progDesc "Run the bot with an auth token")
       )
  <> Opt.command
       "cli"
       (Opt.info (pure CLI) (Opt.progDesc "Run the bot in the CLI"))
  )

parserInfo :: Opt.ParserInfo Command
parserInfo = Opt.info
  (mainParser Opt.<**> Opt.helper)
  (Opt.fullDesc <> Opt.progDesc "Print a greeting for TARGET" <> Opt.header
    "hello - a test for optparse-applicative"
  )
