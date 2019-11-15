{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NumericUnderscores #-}

module EventHandlers.HTTP where

import EventHandlers.Common
import BotServer.API
import BotServer.Model
import BotServer.Core
import BotClientState
import Prelude
import Data.Text(Text, pack)
import qualified Data.Map as Map
import Control.Monad(join)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (STM, atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Network.HTTP.Client(Manager)
import Network.Wai.Handler.Warp (run)
import Servant

type AppM = ReaderT State Handler

type EventAPI = "notify" :> ReqBody '[JSON] Event :> PostNoContent '[JSON] NoContent
type StateAPI = "getState" :> Get '[JSON] MutableState
type API = EventAPI :<|> StateAPI

runHTTPEventServer :: GameConfig -> Strategy -> Manager -> BotServerConfig -> Int -> IO ()
runHTTPEventServer gameConfig strategy manager config port = do
    shutdownMVar <- newEmptyMVar
    mutableState <- liftIO $ atomically $ newTVar MutableState {currentRound = 0, history = Map.empty}
    _ <- forkIO $ run port $ app $ initialState gameConfig strategy manager config mutableState shutdownMVar
    takeMVar shutdownMVar
    threadDelay 1_000_000

api :: Proxy API
api = Proxy

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

app :: State -> Application
app s = serve api $ hoistServer api (nt s) server

server :: ServerT API AppM
server = notify :<|> getState
    where notify :: Event -> AppM NoContent
          notify event = do
            liftIO $ print event
            state <- ask
            let State {shutdownMVar, mutableState = mState} = state
            liftIO $ do
                newRoundOrGameEndMsg <- atomically $ updateMutableState state mState event
                case newRoundOrGameEndMsg of
                    Left currentRound -> do
                        let State{strategy} = state
                        move <- computeMove mState strategy
                        makeMove state currentRound move
                    Right msg -> do 
                        putMVar shutdownMVar ()
                        print msg
            return NoContent
          getState :: AppM MutableState
          getState = do
            State{mutableState} <- ask
            liftIO $ readTVarIO mutableState