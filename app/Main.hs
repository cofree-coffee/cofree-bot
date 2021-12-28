module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Monad
import Control.Monad.Reader
import Data.Coerce
import Network.Matrix.Client
import Options.Applicative qualified as Opt

import CofreeBot

import OptionsParser
import Control.Concurrent.STM.TChan (newTChanIO)

{-
*This is a very scrappy rough draft*

The basic idea is to have two main threads for calling out to the Matrix API:

1. Poll Thread: Fetches new room events and adds them to a TVar. The
particular data structure that belongs in the TVar is unclear to me at
the moment. We might actually want some kind of TChan setup so that our
subroutines can just pluck off the latest messages.

2. Response Thread: Sends messages back to the matrix API. This thread
fetches new responses from a TChan and sends them to the Matrix API.

The idea here to limit the number of threads hammering the API if we
have a bunch of subroutines running at once.

We then have a third long lived thread called the Dispatch
Thread. This thread scans incoming events and detects subroutine
invocations. When it detects an invocation, it forks a thread with the
desired subroutine which has access to the Event TVar and the Response
TChan.

The more I think about it, the more I think we should use a `Map
RoomID TChan` for the Events. Rather then caching all events they
should be streamed in. Whenever we invoke a subroutine, we dupe the
relevant TChan.
-}

main :: IO ()
main = do
  cache <- newTVarIO mempty
  respChan <- newTChanIO

  command <- Opt.execParser parserInfo
  case command of
    LoginCmd cred -> do
      session <- login cred
      let cfg = Config session cache respChan
      connectAndSend cfg
    TokenCmd TokenCredentials{..} -> do
      session <- createSession (coerce matrixServer) matrixToken
      let cfg = Config session cache respChan
      connectAndSend cfg
  -- void $ forkIO $ runReaderT dispatchThread cfg
  -- void $ forkIO $ runReaderT responseThread cfg
  -- runReaderT pollThread cfg
