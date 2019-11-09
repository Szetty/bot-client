{-
   Bot Server API

   This is a bot API to let bots battle

   OpenAPI Version: 3.0.0
   Bot Server API API version: 1.0.0
   Contact: szederjesiarnold@gmail.com
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : BotServer.Model
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-unused-matches -fno-warn-unused-binds -fno-warn-unused-imports #-}

module BotServer.Model where

import BotServer.Core
import BotServer.MimeTypes

import Data.Aeson ((.:),(.:!),(.:?),(.=))

import qualified Control.Arrow as P (left)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (Typeable, TypeRep, typeOf, typeRep)
import qualified Data.Foldable as P
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Time as TI
import qualified Lens.Micro as L
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Control.Applicative ((<|>), Alternative)
import Data.Function ((&))
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Char (toLower)
import Prelude (($),(/=),(.),(<$>),(<*>),(>>=),(=<<),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import GHC.Generics (Generic)

import qualified Prelude as P

-- * Models

-- ** Error
-- | Error
newtype Error = Error
  { message :: Text
  } deriving (P.Show, P.Eq, P.Typeable, Generic)
  
instance A.FromJSON Error
instance A.ToJSON Error

-- ** GameFinishedBody
-- | GameFinishedBody
data GameFinishedBody = GameFinishedBody
  { gameId :: Text
  , score :: Text
  , gameResult :: GameFinishedGameResult
  } deriving (P.Show, P.Eq, P.Typeable, Generic)

instance A.FromJSON GameFinishedBody
instance A.ToJSON GameFinishedBody

-- ** GameFinishedGameResult
-- | GameFinishedGameResult
data GameFinishedGameResult = GameFinishedGameResult
  { status :: !EStatus -- ^ "status"
  , winner :: !Text -- ^ "winner"
  } deriving (P.Show, P.Eq, P.Typeable, Generic)

instance A.FromJSON GameFinishedGameResult
instance A.ToJSON GameFinishedGameResult

-- ** HelloRequest
-- | HelloRequest
data HelloRequest = HelloRequest
  { game :: HelloRequestGame
  , playerName :: Maybe Text
  , eventCallback :: Text
  } deriving (P.Show, P.Eq, P.Typeable, Generic)

instance A.FromJSON HelloRequest
instance A.ToJSON HelloRequest

-- ** HelloRequestGame
-- | HelloRequestGame
data HelloRequestGame = HelloRequestGame
  { name :: GameName
  , connectionToken :: Text
  , numberOfTotalPlayers :: Maybe Int
  } deriving (P.Show, P.Eq, P.Typeable, Generic)
  
instance A.FromJSON HelloRequestGame
instance A.ToJSON HelloRequestGame

-- ** HelloResponse
-- | HelloResponse
data HelloResponse = HelloResponse
  { gameId :: Text
  , player :: HelloResponsePlayer
  , rounds :: Int
  } deriving (P.Show, P.Eq, P.Typeable, Generic)

instance A.FromJSON HelloResponse
instance A.ToJSON HelloResponse

-- ** HelloResponsePlayer
-- | HelloResponsePlayer
data HelloResponsePlayer = HelloResponsePlayer
  { id :: Text
  , name :: Text
  } deriving (P.Show, P.Eq, P.Typeable, Generic)

instance A.FromJSON HelloResponsePlayer
instance A.ToJSON HelloResponsePlayer

-- ** Event
-- | Event
data Event = StartGame StartGameBody 
  | RoundFinished RoundFinishedBody 
  | GameFinished GameFinishedBody 
  deriving(P.Show, Generic)

eventTaggedObject :: A.SumEncoding
eventTaggedObject = A.TaggedObject
  { A.tagFieldName      = "type"
  , A.contentsFieldName = "body"
  }

eventCustomOptions :: A.Options
eventCustomOptions = A.defaultOptions
   { A.sumEncoding = eventTaggedObject,
     A.constructorTagModifier = \(h:t) -> toLower h : t
   }

instance A.FromJSON Event where parseJSON = A.genericParseJSON eventCustomOptions
instance A.ToJSON Event where toJSON = A.genericToJSON eventCustomOptions

-- ** PlayRequest
-- | PlayRequest
data PlayRequest = PlayRequest
  { gameId :: Text
  , playerId :: Text
  , round :: Int
  , move :: Move
  } deriving (P.Show, P.Eq, P.Typeable, Generic)

instance A.FromJSON PlayRequest
instance A.ToJSON PlayRequest

-- ** PlayResponse
-- | PlayResponse
data PlayResponse = PlayResponse
  { round :: Int
  , playersYetToMakeMove :: [Text]
  } deriving (P.Show, P.Eq, P.Typeable, Generic)

instance A.FromJSON PlayResponse
instance A.ToJSON PlayResponse

-- ** Move
-- | Move
newtype Move = RPSMove RockPaperScissorsMove
  deriving(P.Show, P.Eq, P.Typeable, Generic)

instance A.FromJSON Move
instance A.ToJSON Move

-- ** RockPaperScissorsMove
-- | RockPaperScissorsMove
newtype RockPaperScissorsMove = RockPaperScissorsMove
  { value :: EValue
  } deriving (P.Show, P.Eq, P.Typeable, Generic)

instance A.FromJSON RockPaperScissorsMove
instance A.ToJSON RockPaperScissorsMove

-- ** RoundFinishedBody
-- | RoundFinishedBody
data RoundFinishedBody = RoundFinishedBody
  { gameId :: Text
  , currentRound :: Int
  , roundResult :: RoundFinishedRoundResult
  , nextRound :: Int
  , score :: Text
  } deriving (P.Show, P.Eq, P.Typeable, Generic)

instance A.FromJSON RoundFinishedBody
instance A.ToJSON RoundFinishedBody

-- ** RoundFinishedRoundResult
-- | RoundFinishedRoundResult
data RoundFinishedRoundResult = RoundFinishedRoundResult
  { status :: EStatus
  , winner :: Maybe Text
  , moves :: Maybe (Map.Map Text Move)
  } deriving (P.Show, P.Eq, P.Typeable, Generic)

instance A.FromJSON RoundFinishedRoundResult
instance A.ToJSON RoundFinishedRoundResult

-- ** StartGameBody
-- | StartGameBody
data StartGameBody = StartGameBody
  { gameId :: Text
  , players :: [Text]
  , nextRound :: Int
  } deriving (P.Show, P.Eq, P.Typeable, Generic)

instance A.FromJSON StartGameBody
instance A.ToJSON StartGameBody

-- * Enums


-- ** GameName

-- | Enum of 'Text'
data GameName
  = RPS -- ^ @"rps"@
  deriving (P.Show, P.Eq, P.Typeable, P.Ord, P.Bounded, P.Enum)

instance A.ToJSON GameName where toJSON = A.toJSON . fromGameName
instance A.FromJSON GameName where parseJSON o = P.either P.fail pure . toGameName =<< A.parseJSON o
instance WH.ToHttpApiData GameName where toQueryParam = WH.toQueryParam . fromGameName
instance WH.FromHttpApiData GameName where parseQueryParam o = WH.parseQueryParam o >>= P.left T.pack . toGameName
instance MimeRender MimeMultipartFormData GameName where mimeRender _ = mimeRenderDefaultMultipartFormData

-- | unwrap GameName enum
fromGameName :: GameName -> Text
fromGameName = \case
  RPS -> "rps"

-- | parse 'E'Name' enum
toGameName :: Text -> P.Either String GameName
toGameName = \case
  "rps" -> P.Right RPS
  s -> P.Left $ "toGameName: enum parse failure: " P.++ P.show s


-- ** E'Status

-- | Enum of 'Text'
data EStatus
  = EStatusDraw -- ^ @"draw"@
  | EStatusWin -- ^ @"win"@
  | EStatusLose -- ^ @"lose"@
  deriving (P.Show, P.Eq, P.Typeable, P.Ord, P.Bounded, P.Enum)

instance A.ToJSON EStatus where toJSON = A.toJSON . fromEStatus
instance A.FromJSON EStatus where parseJSON o = P.either P.fail pure . toEStatus =<< A.parseJSON o
instance WH.ToHttpApiData EStatus where toQueryParam = WH.toQueryParam . fromEStatus
instance WH.FromHttpApiData EStatus where parseQueryParam o = WH.parseQueryParam o >>= P.left T.pack . toEStatus
instance MimeRender MimeMultipartFormData EStatus where mimeRender _ = mimeRenderDefaultMultipartFormData

-- | unwrap 'EStatus' enum
fromEStatus :: EStatus -> Text
fromEStatus = \case
  EStatusDraw -> "draw"
  EStatusWin -> "win"
  EStatusLose -> "lose"

-- | parse 'EStatus' enum
toEStatus :: Text -> P.Either String EStatus
toEStatus = \case
  "draw" -> P.Right EStatusDraw
  "win" -> P.Right EStatusWin
  "lose" -> P.Right EStatusLose
  s -> P.Left $ "toEStatus: enum parse failure: " P.++ P.show s


-- ** EValue

-- | Enum of 'Text'
data EValue
  = EValueRock -- ^ @"rock"@
  | EValuePaper -- ^ @"paper"@
  | EValueScissors -- ^ @"scissors"@
  deriving (P.Show, P.Eq, P.Typeable, P.Ord, P.Bounded, P.Enum)

instance A.ToJSON EValue where toJSON = A.toJSON . fromEValue
instance A.FromJSON EValue where parseJSON o = P.either P.fail pure . toEValue =<< A.parseJSON o
instance WH.ToHttpApiData EValue where toQueryParam = WH.toQueryParam . fromEValue
instance WH.FromHttpApiData EValue where parseQueryParam o = WH.parseQueryParam o >>= P.left T.pack . toEValue
instance MimeRender MimeMultipartFormData EValue where mimeRender _ = mimeRenderDefaultMultipartFormData

-- | unwrap 'EValue' enum
fromEValue :: EValue -> Text
fromEValue = \case
  EValueRock -> "rock"
  EValuePaper -> "paper"
  EValueScissors -> "scissors"

-- | parse 'EValue' enum
toEValue :: Text -> P.Either String EValue
toEValue = \case
  "rock" -> P.Right EValueRock
  "paper" -> P.Right EValuePaper
  "scissors" -> P.Right EValueScissors
  s -> P.Left $ "toEValue: enum parse failure: " P.++ P.show s