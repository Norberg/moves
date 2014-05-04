{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell #-}

import Moves
import Test.HUnit
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time.Clock


tests = 
 [
    TestCase $ do
        json <- getJSON "test/json/user_places_daily?pastDays=3"
        let (Right decoded) = Moves.decodeJSON json
        assertEqual "date is" "20140429" (date $ head decoded)
        assertEqual "nr of entries are" 3 (length decoded)
        assertEqual "nr of segments are" 7 (length $ segments $ head decoded)
        let sTime = startTime $ head $ segments $ head decoded
        assertEqual "start time is" "20140428T212751+0200" (sTime)
        let placeName = name $ place $  head $ segments $ head decoded
        assertEqual "placeName is" (Just "Home") (placeName)
    ,
    TestCase $ do
        json <- getJSON "test/json/user_places_daily?pastDays=3"
        let (Right decoded) = Moves.decodeJSON json
        let work = filterPlace "Work" $ segments $ head decoded
        assertEqual "nr of work entries are" 2 (length work)
        let work1 = head work
        let work2 = work !! 1
        let startTime1 = decodeStartTime work1
        let endTime1 = decodeEndTime work1
        assertEqual "startTime is" (read "2014-04-29 05:39:14 UTC") startTime1
        assertEqual "endTime is" (read "2014-04-29 09:20:41 UTC") endTime1
        let duration1 = getDuratation work1
        assertEqual "duration is" 13287 duration1
        let duration2 = getDuratation work2
        assertEqual "duration is" 16398 duration2
        let totalDuration = sumDurration work /3600
        assertEqual "Total work hours are" 8.245833333333334 totalDuration
    ,
    TestCase $ do
        json <- getJSON "test/json/user_places_daily?pastDays=3"
        let (Right decoded) = Moves.decodeJSON json
        let workSegments = map (filterPlaceOnEntry "Work") decoded
        assertEqual "nr of workSegments are" 3 (length workSegments)
        let totalDurationPerDay = map sumDurration workSegments 
        let dates = map (decodeJSONDate . date) decoded
        let ziped = zip dates totalDurationPerDay
        let formated = map formatedDailyDurration ziped
        assertEqual "daily summary is " [29685.0,27757.0,0.0] totalDurationPerDay
        assertEqual "formated summary is " ["Tuesday April 29: 8.25 hours"
                                           ,"Wednesday April 30: 7.71 hours",
                                            "Thursday May 01: 0.00 hours"] formated
    
 ]

main = runTestTT $ TestList tests


getJSON :: FilePath -> IO BL.ByteString
getJSON jsonFile = BL.readFile jsonFile    
