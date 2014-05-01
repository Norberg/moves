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

baseUrl :: BS.ByteString
baseUrl = "https://api.moves-app.com/api/1.1/"

get :: AccessToken -> BS.ByteString -> IO (OAuth2Result BL.ByteString)
get token request = authGetBS token $ BS.append baseUrl request

getActivitysLastWeek :: AccessToken -> IO (OAuth2Result BL.ByteString)
getActivitysLastWeek token = get token "user/activities/daily?pastDays=1"
