{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import BotServer.Model
import BotServer.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy Error)
      propMimeEq MimeJSON (Proxy :: Proxy GameFinished)
      propMimeEq MimeJSON (Proxy :: Proxy GameFinishedGameResult)
      propMimeEq MimeJSON (Proxy :: Proxy HelloRequest)
      propMimeEq MimeJSON (Proxy :: Proxy HelloRequestGame)
      propMimeEq MimeJSON (Proxy :: Proxy HelloResponse)
      propMimeEq MimeJSON (Proxy :: Proxy HelloResponsePlayer)
      propMimeEq MimeJSON (Proxy :: Proxy InlineObject)
      propMimeEq MimeJSON (Proxy :: Proxy PlayRequest)
      propMimeEq MimeJSON (Proxy :: Proxy PlayResponse)
      propMimeEq MimeJSON (Proxy :: Proxy RockPaperScissorsMove)
      propMimeEq MimeJSON (Proxy :: Proxy RoundFinished)
      propMimeEq MimeJSON (Proxy :: Proxy RoundFinishedRoundResult)
      propMimeEq MimeJSON (Proxy :: Proxy StartGame)
      
