module Main where

import           Control.Monad
import           Enums
import           IedConnection

simpleList = do
  con <- connect "localhost" 102
  x <- discover con
  forM_ x $ \item -> do
    val <- readVal con (ref item) (fc item)
    putStr $ show item ++ " == "
    print val

main = simpleList
