{-# LANGUAGE OverloadedStrings #-}
import Moves.Core
import Moves.ApiKey

import System.Environment   
import qualified Data.ByteString.Char8 as BS

main = do
    args <- getArgs
    result <- get token $ BS.pack $ head args
    print result
