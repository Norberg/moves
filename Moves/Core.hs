{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Moves.Core where

import Moves.ApiKey (apiKey, token)
import Network.OAuth.OAuth2
import Network.OAuth.OAuth2.HttpClient

import Prelude
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson
import GHC.Generics
import Data.Time.Clock
import Data.Time
import Data.Time.Format
import System.Locale
import Text.Printf


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
