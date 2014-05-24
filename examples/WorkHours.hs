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
{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell #-}
import System.Directory
import System.FilePath

import Moves.ApiKey (token)
import Moves.Core
import Moves.Places
import Moves.Utils

main = do
    workplace <- getWorkplace
    (Right json) <- get token "/user/places/daily?pastDays=10"
    let decodeResult = decode json
    let (Right activities) = decodeResult
    let workSegments = map (filterPlaceOnEntry workplace) activities
    let totalDurationPerDay = map sumDurration workSegments 
    let dates = map (decodeJSONDate . date) activities
    let ziped = zip dates totalDurationPerDay
    let formated = map formatedDailyDurration ziped
    mapM_ putStrLn formated

getWorkplace = do
    path <- getHomePath "workplace.cfg"
    fileContent <- readFile path
    return $ filter (/= '\n') fileContent

getHomePath :: String -> IO FilePath
getHomePath s = do
    homeDir <- getHomeDirectory
    return (joinPath [homeDir, s])
