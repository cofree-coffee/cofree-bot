module Main where

import CofreeBot
import Network.Matrix.Client
import Options.Applicative

parseToken :: Parser MatrixToken
parseToken =
     MatrixToken <$> strOption
         (long "auth_token"
       <> metavar "MATRIX_AUTH_TOKEN"
       <> help "Matrix authentication token")

parseServer :: Parser MatrixServer
parseServer =
     MatrixServer <$> strOption
         (long "homeserver"
       <> metavar "MATRIX_HOMESERVER"
       <> help "Matrix Homeserver")

parseConfig :: Parser Config
parseConfig = Config <$> parseToken <*> parseServer

main :: IO ()
main = connectAndListen =<< execParser opts
  where
    opts = info (parseConfig <**> helper)
           (fullDesc
             <> progDesc "Print a greeting for TARGET"
             <> header "hello - a test for optparse-applicative" )
