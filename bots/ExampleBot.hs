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
connectionConfig = ConnectionConfig {token = "connectionToken2", clientPort = 9000, serverAddress = "http://localhost:8080"}

main :: IO ()
main = playWithStrategy RPS "ExampleBot" strategy connectionConfig

strategy :: MutableState -> Move
strategy _state = RPSMove RockPaperScissorsMove {value = EValueRock}