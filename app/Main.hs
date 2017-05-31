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
              attributes <- dataObjectDirectoryByFC con attrPath st
              forM_ attributes $
                \attribute -> do
                  let fullPath = attrPath ++ "." ++ attribute
                  let cleanPath = takeWhile (/= '[') fullPath
                  type_ <- mmsType con cleanPath st
                  putStrLn $ fullPath ++ " :: " ++ (show type_)

main = simpleList
