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
{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Exit

import Moves.Core
import Moves.Utils
import Moves.Places as Places
import Moves.Summary as Summary

tests = 
 [
    TestCase $ do
        json <- getJSON "test/json/user_places_daily?pastDays=3"
        let either = Places.decode json
        let decoded = getRightOrExit either
        assertEqual "date is" "20140429" (Places.date $ head decoded)
        assertEqual "nr of entries are" 3 (length decoded)
        assertEqual "nr of segments are" 7 (length $ segments $ head decoded)
        let sTime = Places.startTime $ head $ segments $ head decoded
        assertEqual "start time is" "20140428T212751+0200" (sTime)
        let placeName = Places.name $ Places.place $  head $ segments $ head decoded
        assertEqual "placeName is" (Just "Home") (placeName)
    ,
    TestCase $ do
        json <- getJSON "test/json/user_places_daily?pastDays=3"
        let decoded = Places.decodeOrExit json
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
        let decoded = Places.decodeOrExit json
        let workSegments = map (filterPlaceOnEntry "Work") decoded
        assertEqual "nr of workSegments are" 3 (length workSegments)
        let totalDurationPerDay = map sumDurration workSegments 
        let dates = map (decodeJSONDate . Places.date) decoded
        let ziped = zip dates totalDurationPerDay
        let formated = map formatedDailyDurration ziped
        assertEqual "daily summary is " [29685.0,27757.0,0.0] totalDurationPerDay
        assertEqual "formated summary is " ["Tuesday April 29: 8.25 hours"
                                           ,"Wednesday April 30: 7.71 hours",
                                            "Thursday May 01: 0.00 hours"] formated
    ,
    TestCase $ do
        json <- getJSON "test/json/user_summary_daily_2014W20"
        let decoded = Summary.decodeOrExit json
        assertEqual "nr of entries are" 7 (length decoded)
        let lastEntry = last decoded
        let expectedEntry = Summary.Entry{ Summary.date = "20140518"
                                            ,summary = [
                                                Summary {activity = "walking"
                                                , group = "walking"
                                                , duration = 30
                                                , distance = 15,
                                                 steps = Just 30}]
                                            , Summary.lastUpdate = "20140519T061410Z"}

        assertEqual "verify that last entry is " expectedEntry lastEntry
    ]


main = do
    result <- runTestTT $ TestList tests
    let errs = errors result
        fails = failures result
    System.Exit.exitWith (codeGet errs fails)

codeGet errs fails
 | fails > 0       = ExitFailure 2
 | errs > 0        = ExitFailure 1
 | otherwise       = ExitSuccess
 
getJSON :: FilePath -> IO BL.ByteString
getJSON jsonFile = BL.readFile jsonFile    
