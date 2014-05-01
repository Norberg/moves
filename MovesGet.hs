{-# LANGUAGE OverloadedStrings #-}
import Moves
import ApiKey

import System.Environment   
import qualified Data.ByteString.Char8 as BS


main = do
    args <- getArgs
    result <- Moves.get token $ BS.pack $ head args
    print result
    

--print $ result args 
--    where
--          result [] = "You must specify a argument to get any data!" :: BS.ByteString
--          result xs = Moves.get token $ BS.pack $ head xs
