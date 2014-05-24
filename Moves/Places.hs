{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Moves.Places(
    Location(..),
    Place(..),
    Segment(..),
    Entry(..),
    decode,
    decodeOrExit,
    filterPlace,
    filterPlaceOnEntry,
    decodeStartTime,
    decodeEndTime,
    getDuratation,
    sumDurration
) where

import Prelude
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson(FromJSON, ToJSON, eitherDecode)
import GHC.Generics
import Data.Time.Clock

import Moves.Core
import Moves.Utils

data Location = Location {
  lat :: Double
, lon :: Double 
} deriving (Show, Generic, Eq)
instance FromJSON Location
instance ToJSON Location

data Place = Place {
  id :: Int
, name :: Maybe String
-- type :: String
, foursquareId :: Maybe String
, location :: Location
} deriving (Show, Generic, Eq)

instance FromJSON Place
instance ToJSON Place

data Segment = Segment {
-- type :: String
  startTime :: String
, endTime :: String
, place :: Place
, lastUpdate :: String
} deriving (Show, Generic, Eq)

instance FromJSON Segment
instance ToJSON Segment

data Entry = Entry {
  date :: String
, segments :: [Segment] 
} deriving (Show, Generic, Eq)

instance FromJSON Entry
instance ToJSON Entry

decode :: BL.ByteString -> (Either String [Entry])
decode json = eitherDecode json

decodeOrExit :: BL.ByteString -> [Entry]
decodeOrExit json = getRightOrExit $ decode json


filterPlace :: String -> [Segment] -> [Segment]
filterPlace placeName segments = filter (f placeName) segments where
    f p x = (Just p) == (name $ place x)

filterPlaceOnEntry :: String -> Entry ->  [Segment]
filterPlaceOnEntry p e = filterPlace p $ segments e

decodeStartTime :: Segment -> UTCTime
decodeStartTime seg = decodeJSONDateTime $ startTime seg

decodeEndTime :: Segment -> UTCTime
decodeEndTime seg = decodeJSONDateTime $ endTime seg

-- Returns duration of the segment in seconds
getDuratation :: Segment -> Double
getDuratation seg = realToFrac $ diffUTCTime endTime startTime where
        startTime = decodeStartTime seg
        endTime = decodeEndTime seg

-- Return total durration of all segments
sumDurration :: [Segment] -> Double
sumDurration segs = sum $ map getDuratation segs

