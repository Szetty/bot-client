{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NumericUnderscores #-}

module EventHandlers.WS where

import EventHandlers.Common
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import qualified Data.Text           as T
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS
import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Network.URI
import BotServer.API
import BotServer.Model
import BotServer.Core
import BotClientState
import Prelude
import Data.Text(Text, pack)
import qualified Data.Map as Map
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (STM, atomically)
import Control.Concurrent.STM.TVar
import Network.HTTP.Client(Manager)
import Data.List(stripPrefix)
import Data.Aeson(decode)

runWSEventServer :: GameConfig -> Strategy -> Manager -> BotServerConfig -> IO ()
runWSEventServer gameConfig strategy manager config = do
    let GameConfig {gameId, playerId} = gameConfig
    let BotServerConfig {configHost} = config
    shutdownMVar <- newEmptyMVar
    mutableState <- liftIO $ atomically $ newTVar MutableState {currentRound = 0, history = Map.empty}
    let path = "/ws?gameId=" ++ show gameId ++ "&playerId=" ++ show playerId
    let state = initialState gameConfig strategy manager config mutableState shutdownMVar
    case parseConfigHost configHost of
        Nothing -> print $ "Could not parse configHost " ++ show configHost
        Just (host, port) -> do
            _ <- forkIO $ withSocketsDo $ WS.runClient host port path $ app state
            takeMVar shutdownMVar
            threadDelay 1_000_000
    

app :: State -> WS.ClientApp ()
app state conn = do
    putStrLn "Connected to WS"

    forever $ do
        msg <- WS.receiveData conn
        case decode msg of
            Nothing -> print "Received unknown message"
            Just event -> do
                liftIO $ print event
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

    WS.sendClose conn ("Bye!" :: Text)
    
parseConfigHost :: BCL.ByteString -> Maybe (String, Int)
parseConfigHost configHost =
    case parseURI $ BCL.unpack configHost of
        Nothing -> Nothing
        Just URI{uriAuthority} ->
            case uriAuthority of
                Nothing -> Nothing
                Just URIAuth{uriRegName, uriPort} -> 
                    case stripPrefix ":" uriPort of
                        Nothing -> Just (uriRegName, 80)
                        Just portS -> Just (uriRegName, read portS :: Int)
