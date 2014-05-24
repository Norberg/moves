{-
Copyright (c) 2014, Simon Norberg

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Simon Norberg nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
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

