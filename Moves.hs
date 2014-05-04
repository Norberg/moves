{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Moves where

import ApiKey (apiKey, token)
import Network.OAuth.OAuth2
import Network.OAuth.OAuth2.HttpClient

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

createAccessToken :: IO ()
createAccessToken = do
    print $ authorizationUrl apiKey `appendQueryParam` scope
    putStrLn "visit the url and paste code here: "
    code <- fmap BS.pack getLine
    let (url, body) = accessTokenUrl apiKey code
    (Right newToken) <- doJSONPostRequest url body
    print $ newToken
    getActivitysLastWeek newToken >>= print

getActivitys :: IO ()
getActivitys = do
    getActivitysLastWeek token >>= print

scope :: QueryParams
scope = [("scope", "activity location")]

baseUrl :: BS.ByteString
baseUrl = "https://api.moves-app.com/api/1.1"

get :: AccessToken -> BS.ByteString -> IO (OAuth2Result BL.ByteString)
get token request = authGetBS token $ BS.append baseUrl request

getActivitysLastWeek :: AccessToken -> IO (OAuth2Result BL.ByteString)
getActivitysLastWeek token = get token "/user/activities/daily?pastDays=7"


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

decodeJSON :: BL.ByteString -> [Entry]
decodeJSON json = decoded
    where (Right decoded) = eitherDecode json :: (Either String [Entry])
          (Left e) = error e

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
     
