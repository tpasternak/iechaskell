-- This demo reads all MMS variables from a mms server running on localhost:102
module Main where

import           Control.Monad
import           Iec61850.Client
import           Data.Data
import           Data.Either.Utils(fromRight)
import           Control.Monad.Except(runExceptT)
import           System.IO(stderr,hPutStr)
simpleList = do
  connection <- runExceptT $ connect "localhost" 102
  case connection of
    Right con -> do
      x <-  discover con
      forM_ x $ \(ref, fc) -> do
        val <- readVal con ref fc
        putStr $ ref ++ "[" ++ show fc ++ "]" ++ " == " ++ show (toConstr val) ++ ": "
        print val
    Left err -> hPutStr stderr err

main = simpleList
