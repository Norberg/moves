{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Moves.Summary(
    Summary(..),
    Entry(..),
    decode,
    decodeOrExit
) where

import Prelude
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson(FromJSON, ToJSON, eitherDecode)
import GHC.Generics

import Moves.Core

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
