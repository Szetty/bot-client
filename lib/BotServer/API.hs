{-
   Bot Server API

   This is a bot API to let bots battle

   OpenAPI Version: 3.0.0
   Bot Server API API version: 1.0.0
   Contact: szederjesiarnold@gmail.com
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : BotServer.API
-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BotServer.API
  ( module BotServer.API.Connect
  , module BotServer.API.Play
  , connect
  , sendMove
  ) where

import BotServer.API.Connect
import BotServer.API.Play
import BotServer.Client
import BotServer.Core
import BotServer.Model
import Network.HTTP.Client
import Prelude
import Data.Text(Text)
import Data.Aeson(decode)
import Data.Maybe

connect :: Manager -> BotServerConfig -> GameName -> Maybe Int -> Text -> Maybe Text -> Text -> IO (Either Text HelloResponse)
connect mgr config gameName totalRounds playerName eventCallback connectionToken = do
  let helloRequestGame = HelloRequestGame{name = gameName, connectionToken = connectionToken, numberOfTotalPlayers = Nothing, totalRounds = totalRounds}
  let connectRequest = helloPost HelloRequest{game = helloRequestGame, eventCallback = eventCallback, playerName = Just playerName}
  fmap handleResponse (dispatchMime mgr config connectRequest)

sendMove :: Manager -> BotServerConfig -> Text -> Text -> Int -> Move -> IO ()
sendMove mgr config gameId playerId currentRound move = do
  let playRequest = playPost PlayRequest{ gameId = gameId, playerId = playerId, round = currentRound, move = move}
  playResponse <- fmap handleResponse (dispatchMime mgr config playRequest)
  case playResponse of
    Left msg ->
      print msg
    Right PlayResponse{round = playRound, playersYetToMakeMove} ->
      print $ "In round " ++ show playRound ++ " the following players need to make a move: " ++ show playersYetToMakeMove

handleResponse :: MimeResult b -> Either Text b
handleResponse mimeResponse = do
  let MimeResult {mimeResult = result} = mimeResponse
  case result of
      Left MimeError {mimeErrorResponse = response} -> 
          Left $ fromJust ((decode . responseBody) response >>= (\BotServer.Model.Error{ message = msg } -> Just msg))
      Right response -> Right response