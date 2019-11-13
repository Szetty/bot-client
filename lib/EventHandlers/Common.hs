{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NumericUnderscores #-}

module EventHandlers.Common where

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

data GameConfig = GameConfig {
    gameId :: Text
    , playerId :: Text
    , playerName :: Text
}

data State = State {
    gameConfig :: GameConfig
    , mutableState :: TVar MutableState
    , strategy :: Strategy
    , manager :: Manager
    , config :: BotServerConfig
    , shutdownMVar :: MVar ()
}

initialState :: GameConfig -> Strategy -> Manager -> BotServerConfig -> TVar MutableState -> MVar () -> State
initialState gameConfig strategy manager config mutableState shutdownMVar =
    State{
        gameConfig = gameConfig,
        strategy = strategy,
        mutableState = mutableState,
        manager = manager,
        config = config,
        shutdownMVar = shutdownMVar
    }

updateMutableState :: State -> TVar MutableState -> Event -> STM (Either Int Text)
updateMutableState state mState event = do
    mutableState <- readTVar mState
    let stateEither = handleEvent state mutableState event
    case stateEither of
        Left newMutableState -> do
            let MutableState {currentRound} = newMutableState
            writeTVar mState newMutableState
            return $ Left currentRound
        Right message ->
            return $ Right message

handleEvent :: State -> MutableState -> Event -> Either MutableState Text
handleEvent state mutableState event =
    case event of
        StartGame StartGameBody {nextRound} ->
            Left (mutableState {currentRound = nextRound} :: MutableState)
        RoundFinished RoundFinishedBody {currentRound, nextRound, roundResult = RoundFinishedRoundResult{moves}} -> do
            let MutableState{history} = mutableState
            let updatedHistory = updateHistory state history currentRound moves
            Left (mutableState {currentRound = nextRound} :: MutableState)
        GameFinished GameFinishedBody {score, gameResult = GameFinishedGameResult{winner}} -> 
            Right $ pack $ "The score is " ++ show score ++ " and the winner is " ++ show winner

makeMove :: State -> Int -> Move -> IO ()
makeMove State{manager, config, gameConfig = GameConfig {gameId, playerId}} = sendMove manager config gameId playerId

updateHistory :: State -> Map.Map Int [Move] -> Int -> Maybe (Map.Map Text Move) -> Map.Map Int [Move]
updateHistory state history currentRound movesMaybe = do
    let State {gameConfig = GameConfig{playerName}} = state
    case movesMaybe of
        Just movesMap -> do
            let movesWithoutOwnMove = Map.elems (Map.delete playerName movesMap)
            Map.update (\moves -> Just (moves ++ movesWithoutOwnMove)) currentRound history
        Nothing -> 
            history

computeMove :: TVar MutableState -> Strategy -> IO Move
computeMove mState strategy=
    join $ atomically $ do
        mutableState <- readTVar mState
        return $ strategy mutableState