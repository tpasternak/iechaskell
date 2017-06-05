module Main where

import           Control.Monad
import           Enums
import           IedConnection

simpleList = do
  x <- discover "localhost"
  forM_ x print

main = simpleList
