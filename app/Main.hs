module Main where

import           Control.Monad
import           Client

simpleList = do
  con <- connect "localhost" 102
  x <- discover con
  forM_ x $ \(ref, fc) -> do
    val <- readVal con ref fc
    putStr $ ref ++ "[" ++ show fc ++ "]" ++ " == "
    print val

main = simpleList
