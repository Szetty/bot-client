{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NumericUnderscores #-}

module EventHandler where

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
import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Network.HTTP.Client(Manager)
import Network.Wai.Handler.Warp (run)
import Servant

eventCallback :: Int -> Text
eventCallback port = pack $ "http://localhost:" ++ show port ++ "/notify"

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

type AppM = ReaderT State Handler

type EventAPI = "notify" :> ReqBody '[JSON] Event :> PostNoContent '[JSON] NoContent
type StateAPI = "getState" :> Get '[JSON] MutableState
type API = EventAPI :<|> StateAPI

runEventServer :: GameConfig -> Strategy -> Manager -> BotServerConfig -> Int -> IO ()
runEventServer gameConfig strategy manager config port = do
    shutdownMVar <- newEmptyMVar
    mutableState <- liftIO $ atomically $ newTVar MutableState {currentRound = 0, history = Map.empty}
    _ <- forkIO $ run port $ app $ initialState gameConfig strategy manager config mutableState shutdownMVar
    takeMVar shutdownMVar
    threadDelay 1_000_000

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
                moveOrMsg <- atomically $ updateMutableState state mState event
                case moveOrMsg of
                    Left (currentRound, move) -> makeMove state currentRound move
                    Right msg -> do 
                        putMVar shutdownMVar ()
                        print msg
            return NoContent
          getState :: AppM MutableState
          getState = do
            State{mutableState} <- ask
            liftIO $ readTVarIO mutableState

updateMutableState :: State -> TVar MutableState -> Event -> STM (Either (Int, Move) Text)
updateMutableState state mState event = do
    mutableState <- readTVar mState
    let stateEither = handleEvent state mutableState event
    case stateEither of
        Left (newMutableState, move) -> do
            let MutableState {currentRound} = newMutableState
            writeTVar mState newMutableState
            return $ Left (currentRound, move)
        Right message ->
            return $ Right message

handleEvent :: State -> MutableState -> Event -> Either (MutableState, Move) Text
handleEvent state mutableState event =
    case event of
        StartGame StartGameBody {nextRound} -> do
            let State{strategy} = state
            Left $ updateStateAndGetMove mutableState strategy nextRound
        RoundFinished RoundFinishedBody {currentRound, nextRound, roundResult = RoundFinishedRoundResult{moves}} -> do
            let MutableState{history} = mutableState
            let State{strategy} = state
            let updatedHistory = updateHistory state history currentRound moves
            Left $ updateStateAndGetMove mutableState{history = updatedHistory} strategy nextRound
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
    

updateStateAndGetMove :: MutableState -> Strategy -> Int -> (MutableState, Move)
updateStateAndGetMove mutableState strategy currentRound = do
    let MutableState {history} = mutableState
    let move = strategy mutableState
    let updatedHistory = Map.insert currentRound [move] history
    (MutableState{history = updatedHistory, currentRound = currentRound}, move)