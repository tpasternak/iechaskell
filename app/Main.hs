module Main where

import Iec61850
import Enums
import Control.Monad

simpleList = do
  con <- connect "localhost" 102
  ldevices <- logicalDevices con
  forM_ ldevices $
    \dev -> do
      nodes <- logicalNodes con dev
      forM_ nodes $
        \node -> do
          let nodeFull = dev ++ "/" ++ node
          objects <- logicalNodeDirectory con nodeFull dataObject
          forM_ objects $
            \object -> do
              let attrPath = nodeFull ++ "." ++ object
              attributes <- dataObjectDirectory con attrPath dataObject
              forM_ attributes $
                \attribute -> 
                  putStrLn $ attrPath ++ "." ++ attribute

main = simpleList
