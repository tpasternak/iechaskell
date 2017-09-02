-- This demo reads all MMS variables from a mms server running on localhost:102
module Main where

import           Control.Monad
import           Iec61850.Client
import           Data.Data
import           Data.Either.Utils(fromRight)

simpleList = do
  con <- fromRight <$> connect "localhost" 102
  x <-  discover con
  forM_ x $ \(ref, fc) -> do
    val <- readVal con ref fc
    putStr $ ref ++ "[" ++ show fc ++ "]" ++ " == " ++ show (toConstr val) ++ ": "
    print val

main = simpleList
