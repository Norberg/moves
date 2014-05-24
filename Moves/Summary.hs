{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Moves.Summary where

import Moves.Core
import Moves.ApiKey (apiKey, token)
import Network.OAuth.OAuth2
import Network.OAuth.OAuth2.HttpClient

import Prelude
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson(FromJSON, ToJSON, eitherDecode)
import GHC.Generics
import Data.Time.Clock
import Data.Time
import Data.Time.Format
import System.Locale
import Text.Printf


data Summary = Summary {
  activity :: String
, group  :: String
, duration :: Int
, distance :: Int
, steps :: Maybe Int
} deriving (Show, Generic, Eq)

instance FromJSON Summary
instance ToJSON Summary

data Entry = Entry {
  date :: String
, summary :: [Summary]
, lastUpdate :: String 
} deriving (Show, Generic, Eq)

instance FromJSON Entry
instance ToJSON Entry

decode :: BL.ByteString -> (Either String [Entry])
decode json = eitherDecode json

decodeOrExit :: BL.ByteString -> [Entry]
decodeOrExit json = getRightOrExit $ decode json
