{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

import BotServer
import Data.Text()

connectionConfig :: ConnectionConfig
connectionConfig = ConnectionConfig {
    token = "connectionToken2",
    clientPort = 9000,
    serverAddress = "http://localhost:8090",
    connectionType = WS
}

main :: IO ()
main = playWithStrategy GameConfig {name = RPS, totalRounds = Nothing} "ExampleBot" strategy connectionConfig

strategy :: MutableState -> IO Move
strategy _state = return $ RPSMove RockPaperScissorsMove {value = EValueRock}