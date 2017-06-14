module Iec61850.NameTree
where

import           Data.List.Split

data DiscoverStruct = Leaf String
                    | Node String [DiscoverStruct]
  deriving (Eq, Show)

treeFromPathListH :: [[String]] -> [DiscoverStruct]
treeFromPathListH split_ =
  let roots = concat $ filter ((== 1) . length) split_
  in map
     (\root -> let stringsMatchingRoot = filter ((==root).head) split_
                   newStrings = filter (not . null) $ drop 1 <$> stringsMatchingRoot
               in if length stringsMatchingRoot == 1
                  then Leaf root
                  else Node root $ treeFromPathListH newStrings
     )
     roots

buildNameTree :: [String] -> [DiscoverStruct]
buildNameTree = treeFromPathListH . map (splitOn "$")

leavesPaths :: [DiscoverStruct] -> [String]
leavesPaths = leavesPaths' ""
  where
    leavesPaths' :: String -> [DiscoverStruct] -> [String]
    leavesPaths' path = concatMap (\str ->
                                           case str of
                                             Leaf name -> [path ++ name]
                                             Node name ds' -> leavesPaths' (path ++ name ++ ".") ds')

