module Main where

import           Control.Monad
import           Enums
import           IedConnection

data AttributeSpec = AttributeSpec {
  ref :: String,
  fc :: FunctionalConstraint
  } deriving(Show)

discover host  = do 
  con <- connect "localhost" 102
  ldevices <- logicalDevices con
  lds <- forM ldevices $
    \dev -> do
      nodes <- logicalNodes con dev
      nds <- forM nodes $
        \node -> do
          let nodeFull = dev ++ "/" ++ node
          objects <- logicalNodeDirectory con nodeFull dataObject
          obs <- forM objects $
            \object -> do
              let attrPath = nodeFull ++ "." ++ object
              costrs <- forM allConstraints $
                \constraint -> do
                  attributes <- dataObjectDirectoryByFC con attrPath constraint
                  forM attributes $
                    \attribute -> do
                      let fullPath = attrPath ++ "." ++ attribute
                      let cleanPath = takeWhile (/= '[') fullPath
                      return $ AttributeSpec cleanPath constraint
              return $ msum costrs
          return $ msum obs
      return $ msum nds
  return (concat lds)
  
simpleList = do
    x <- (discover "localhost") -- $ \x-> putStrLn ((ref x) ++ (show (fc x)))
    forM_ x (\elem -> putStrLn (show elem))

main = simpleList
