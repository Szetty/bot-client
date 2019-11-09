{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module BotClientState where

import BotServer.Model
import Prelude
import Data.Text(Text)
import Data.Aeson(ToJSON)
import GHC.Generics (Generic)
import qualified Data.Map as Map
  
data MutableState = MutableState {
    currentRound :: Int
    , history :: Map.Map Int [Move]
} deriving(Generic)

instance ToJSON MutableState

type Strategy = (MutableState -> Move)