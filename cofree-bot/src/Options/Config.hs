-- | Subroutine for fetching Client Session data from a config file.
module Options.Config
  ( fromConfig,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.Functor
import Data.Functor.Barbie (bpure)
import Data.Yaml qualified as Yaml
import Options.Types (ClientSessionF)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment.XDG.BaseDir (getUserConfigDir)
import System.FilePath

--------------------------------------------------------------------------------

readJSON :: IO (Maybe Aeson.Value)
readJSON = do
  configDir <- getUserConfigDir "cofree-bot"
  void $ createDirectoryIfMissing True configDir
  let path = configDir </> "config"
  doesFileExist path >>= \case
    True -> Yaml.decodeFileThrow path
    False -> pure Nothing

result :: b -> (a -> b) -> Aeson.Result a -> b
result b f = \case
  Aeson.Success a -> f a
  Aeson.Error _ -> b

fromResult :: a -> Aeson.Result a -> a
fromResult a = result a id

fromConfig :: IO (ClientSessionF Maybe)
fromConfig = do
  readJSON
    <&> \case
      Nothing -> bpure Nothing
      Just json -> fromResult (bpure Nothing) $ Aeson.fromJSON json
