{-# LANGUAGE OverloadedStrings #-}

module Moves where

import ApiKey (apiKey, token)
import Network.OAuth.OAuth2
import Network.OAuth.OAuth2.HttpClient

import Prelude
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL

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

getActivitysLastWeek :: AccessToken -> IO (OAuth2Result BL.ByteString)
getActivitysLastWeek token = authGetBS token "https://api.moves-app.com/api/v1/user/activities/daily?pastDays=7"
