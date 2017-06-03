module Main where

import           Control.Monad
import           Enums
import           IedConnection

simpleList = do
  let constraint = st
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
              attributes <- dataObjectDirectoryByFC con attrPath constraint
              forM_ attributes $
                \attribute -> do
                  let fullPath = attrPath ++ "." ++ attribute
                  let cleanPath = takeWhile (/= '[') fullPath
                  type_ <- mmsType con cleanPath constraint
                  val <- readVal con cleanPath constraint
                  putStrLn $ fullPath ++ " :: " ++ (show type_) ++ " = " ++ (show val)

main = simpleList
