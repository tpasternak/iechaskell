module Main where

import           Control.Monad
import           Enums
import           IedConnection

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
              forM_ allConstraints $
                \constraint -> do
                  attributes <- dataObjectDirectoryByFC con attrPath constraint
                  forM_ attributes $
                    \attribute -> do
                      let fullPath = attrPath ++ "." ++ attribute
                      let cleanPath = takeWhile (/= '[') fullPath
                      val <- readVal con cleanPath constraint
                      putStrLn $ fullPath ++ " = " ++ show val

main = simpleList
