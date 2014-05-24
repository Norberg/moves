module Moves.Utils(
    decodeJSONDateTime,
    decodeJSONDate,
    formatedDailyDurration 
) where

import Prelude
import Data.Time.Clock
import Data.Time
import System.Locale
import Text.Printf

decodeJSONDateTime :: String -> UTCTime
decodeJSONDateTime date = readTime defaultTimeLocale "%Y%m%dT%H%M%S%z" date

decodeJSONDate :: String -> UTCTime
decodeJSONDate date = readTime defaultTimeLocale "%Y%m%d" date

formatedDailyDurration :: (UTCTime, Double) -> String
formatedDailyDurration (date, duration) = datefmt ++ ": " ++ hours ++ " hours"
    where
        datefmt = formatTime defaultTimeLocale "%A %B %d" date
        hours = printf "%0.2f" (duration / 3600)
     
