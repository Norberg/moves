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

module Moves.Core(
    createAccessToken,
    get,
    getActivitys,
    getRightOrExit
) where

import Prelude
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.OAuth.OAuth2

import Moves.ApiKey

createAccessToken :: IO ()
createAccessToken = do
    print $ authorizationUrl apiKey `appendQueryParam` scope
    putStrLn "visit the url and paste code here: "
    code <- fmap BS.pack getLine
    let (url, body) = accessTokenUrl apiKey code
    (Right newToken) <- doJSONPostRequest url body
    print $ newToken
    getActivitysLastWeek newToken >>= print

getActivitysLastWeek :: AccessToken -> IO (OAuth2Result BL.ByteString)
getActivitysLastWeek token = get token "/user/activities/daily?pastDays=7"

getActivitys :: IO ()
getActivitys = do
    getActivitysLastWeek token >>= print

scope :: QueryParams
scope = [("scope", "activity location")]

baseUrl :: BS.ByteString
baseUrl = "https://api.moves-app.com/api/1.1"

get :: AccessToken -> BS.ByteString -> IO (OAuth2Result BL.ByteString)
get token request = authGetBS token $ BS.append baseUrl request

getRightOrExit :: Show a => Either a b -> b
getRightOrExit (Right a) = a
getRightOrExit (Left a) = error $ show a
