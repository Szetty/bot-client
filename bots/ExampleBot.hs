{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

import           BotServer
import           Data.Text ()

connectionConfig :: ConnectionConfig
connectionConfig =
  ConnectionConfig
    { token = "connectionToken2"
    , serverAddress = "http://localhost:8090"
    , connectionType = WS
    }

playerConfig :: PlayerConfig
playerConfig =
  PlayerConfig {playerName = "ExampleBot", strategy = Main.strategy}

gameConfig :: GameConfig
gameConfig = GameConfig {name = RPS, totalRounds = Nothing}

main :: IO ()
main = playWithStrategy gameConfig playerConfig connectionConfig

strategy :: MutableState -> IO Move
strategy _state = return $ RPSMove RockPaperScissorsMove {value = EValueRock}
