{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Moves.Places where

import Prelude
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson
import GHC.Generics
import Data.Time.Clock
import Data.Time
import Data.Time.Format
import System.Locale
import Text.Printf


data Location = Location {
  lat :: Double
, lon :: Double 
} deriving (Show, Generic)
instance FromJSON Location
instance ToJSON Location

data Place = Place {
  id :: Int
, name :: Maybe String
-- type :: String
, foursquareId :: Maybe String
, location :: Location
} deriving (Show, Generic)

instance FromJSON Place
instance ToJSON Place

data Segment = Segment {
-- type :: String
  startTime :: String
, endTime :: String
, place :: Place
, lastUpdate :: String
} deriving (Show, Generic)

instance FromJSON Segment
instance ToJSON Segment

data Entry = Entry {
  date :: String
, segments :: [Segment] 
} deriving (Show, Generic)

instance FromJSON Entry
instance ToJSON Entry

decode :: BL.ByteString -> (Either String [Entry])
decode json = eitherDecode json

filterPlace :: String -> [Segment] -> [Segment]
filterPlace placeName segments = filter (f placeName) segments where
    f p x = (Just p) == (name $ place x)

filterPlaceOnEntry :: String -> Entry ->  [Segment]
filterPlaceOnEntry p e = filterPlace p $ segments e

decodeStartTime :: Segment -> UTCTime
decodeStartTime seg = decodeJSONDateTime $ startTime seg

decodeEndTime :: Segment -> UTCTime
decodeEndTime seg = decodeJSONDateTime $ endTime seg

decodeJSONDateTime :: String -> UTCTime
decodeJSONDateTime date = readTime defaultTimeLocale "%Y%m%dT%H%M%S%z" date

decodeJSONDate :: String -> UTCTime
decodeJSONDate date = readTime defaultTimeLocale "%Y%m%d" date

-- Returns duration of the segment in seconds
getDuratation :: Segment -> Double
getDuratation seg = realToFrac $ diffUTCTime endTime startTime where
        startTime = decodeStartTime seg
        endTime = decodeEndTime seg

-- Return total durration of all segments
sumDurration :: [Segment] -> Double
sumDurration segs = sum $ map getDuratation segs

formatedDailyDurration :: (UTCTime, Double) -> String
formatedDailyDurration (date, duration) = datefmt ++ ": " ++ hours ++ " hours"
    where
        datefmt = formatTime defaultTimeLocale "%A %B %d" date
        hours = printf "%0.2f" (duration / 3600)
     
