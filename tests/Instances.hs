{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import BotServer.Model
import BotServer.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (T.pack k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays
    
-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)
    
arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models
 
instance Arbitrary Error where
  arbitrary = sized genError

genError :: Int -> Gen Error
genError n =
  Error
    <$> arbitraryReducedMaybe n -- errorMessage :: Maybe Text
  
instance Arbitrary GameFinished where
  arbitrary = sized genGameFinished

genGameFinished :: Int -> Gen GameFinished
genGameFinished n =
  GameFinished
    <$> arbitrary -- gameFinishedGameId :: Text
    <*> arbitrary -- gameFinishedScore :: Text
    <*> arbitraryReduced n -- gameFinishedGameResult :: GameFinishedGameResult
  
instance Arbitrary GameFinishedGameResult where
  arbitrary = sized genGameFinishedGameResult

genGameFinishedGameResult :: Int -> Gen GameFinishedGameResult
genGameFinishedGameResult n =
  GameFinishedGameResult
    <$> arbitraryReducedMaybe n -- gameFinishedGameResultStatus :: Maybe E'Status
    <*> arbitraryReducedMaybe n -- gameFinishedGameResultWinner :: Maybe Text
  
instance Arbitrary HelloRequest where
  arbitrary = sized genHelloRequest

genHelloRequest :: Int -> Gen HelloRequest
genHelloRequest n =
  HelloRequest
    <$> arbitraryReduced n -- helloRequestGame :: HelloRequestGame
    <*> arbitraryReducedMaybe n -- helloRequestPlayerName :: Maybe Text
    <*> arbitrary -- helloRequestEventCallback :: Text
  
instance Arbitrary HelloRequestGame where
  arbitrary = sized genHelloRequestGame

genHelloRequestGame :: Int -> Gen HelloRequestGame
genHelloRequestGame n =
  HelloRequestGame
    <$> arbitrary -- helloRequestGameName :: E'Name
    <*> arbitrary -- helloRequestGameConnectionToken :: Text
    <*> arbitraryReducedMaybe n -- helloRequestGameNumberOfTotalPlayers :: Maybe Int
  
instance Arbitrary HelloResponse where
  arbitrary = sized genHelloResponse

genHelloResponse :: Int -> Gen HelloResponse
genHelloResponse n =
  HelloResponse
    <$> arbitrary -- helloResponseGameId :: Text
    <*> arbitraryReduced n -- helloResponsePlayer :: HelloResponsePlayer
    <*> arbitrary -- helloResponseRounds :: Int
  
instance Arbitrary HelloResponsePlayer where
  arbitrary = sized genHelloResponsePlayer

genHelloResponsePlayer :: Int -> Gen HelloResponsePlayer
genHelloResponsePlayer n =
  HelloResponsePlayer
    <$> arbitraryReducedMaybe n -- helloResponsePlayerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- helloResponsePlayerName :: Maybe Text
  
instance Arbitrary InlineObject where
  arbitrary = sized genInlineObject

genInlineObject :: Int -> Gen InlineObject
genInlineObject n =
  InlineObject
    <$> arbitraryReducedMaybe n -- inlineObjectType :: Maybe E'Type
    <*> arbitraryReducedMaybe n -- inlineObjectBody :: Maybe OneOfStartGameRoundFinishedGameFinished
  
instance Arbitrary PlayRequest where
  arbitrary = sized genPlayRequest

genPlayRequest :: Int -> Gen PlayRequest
genPlayRequest n =
  PlayRequest
    <$> arbitrary -- playRequestGameId :: Text
    <*> arbitrary -- playRequestPlayerId :: Text
    <*> arbitrary -- playRequestRound :: Int
    <*> arbitraryReduced n -- playRequestMove :: OneOfRockPaperScissorsMove
  
instance Arbitrary PlayResponse where
  arbitrary = sized genPlayResponse

genPlayResponse :: Int -> Gen PlayResponse
genPlayResponse n =
  PlayResponse
    <$> arbitrary -- playResponseRound :: Int
    <*> arbitrary -- playResponsePlayersYetToMakeMove :: [Text]
  
instance Arbitrary RockPaperScissorsMove where
  arbitrary = sized genRockPaperScissorsMove

genRockPaperScissorsMove :: Int -> Gen RockPaperScissorsMove
genRockPaperScissorsMove n =
  RockPaperScissorsMove
    <$> arbitraryReducedMaybe n -- rockPaperScissorsMoveValue :: Maybe E'Value
  
instance Arbitrary RoundFinished where
  arbitrary = sized genRoundFinished

genRoundFinished :: Int -> Gen RoundFinished
genRoundFinished n =
  RoundFinished
    <$> arbitrary -- roundFinishedGameId :: Text
    <*> arbitrary -- roundFinishedCurrentRound :: Int
    <*> arbitraryReduced n -- roundFinishedRoundResult :: RoundFinishedRoundResult
    <*> arbitrary -- roundFinishedNextRound :: Int
    <*> arbitrary -- roundFinishedScore :: Text
  
instance Arbitrary RoundFinishedRoundResult where
  arbitrary = sized genRoundFinishedRoundResult

genRoundFinishedRoundResult :: Int -> Gen RoundFinishedRoundResult
genRoundFinishedRoundResult n =
  RoundFinishedRoundResult
    <$> arbitrary -- roundFinishedRoundResultStatus :: E'Status
    <*> arbitraryReducedMaybe n -- roundFinishedRoundResultWinner :: Maybe Text
  
instance Arbitrary StartGame where
  arbitrary = sized genStartGame

genStartGame :: Int -> Gen StartGame
genStartGame n =
  StartGame
    <$> arbitrary -- startGameGameId :: Text
    <*> arbitrary -- startGamePlayers :: [Text]
    <*> arbitrary -- startGameNextRound :: Int
  



instance Arbitrary E'Name where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Value where
  arbitrary = arbitraryBoundedEnum

