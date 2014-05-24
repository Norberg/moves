{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell #-}
import Moves.ApiKey (token)
import Moves.Core
import Moves.Places
import System.Directory
import System.FilePath

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
