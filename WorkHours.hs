{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell #-}
import Moves
import ApiKey (token)

main = do
    workplace <- getWorkplace
    (Right json) <- get token "/user/places/daily?pastDays=10"
    let decodeResult = decodeJSON json
    let (Right activities) = decodeResult
    let workSegments = map (filterPlaceOnEntry workplace) activities
    let totalDurationPerDay = map sumDurration workSegments 
    let dates = map (decodeJSONDate . date) activities
    let ziped = zip dates totalDurationPerDay
    let formated = map formatedDailyDurration ziped
    mapM_ putStrLn formated

getWorkplace = do
    fileContent <- readFile "workplace.cfg"
    return $ filter (/= '\n') fileContent
