module Iec61850.NameTree
where

import           Data.List.Split

data DiscoverStruct = Leaf String
                    | Node String [DiscoverStruct]
  deriving (Eq, Show)

treeFromPathListH :: [[String]] -> [DiscoverStruct]
treeFromPathListH split_ =
  let roots = concat $ filter ((== 1) . length) split_
      newTrees = map
                   (\root -> let nextString = filter (\tree -> not (null tree) && root == head tree) split_
                                 asn = if length nextString == 1
                                         then Leaf root
                                         else Node root (treeFromPathListH (drop 1 <$> nextString))
                             in asn)
                   roots
  in newTrees

buildNameTree :: [String] -> [DiscoverStruct]
buildNameTree strings =
  let split_ = map (splitOn "$") strings
  in treeFromPathListH split_


leavesPaths :: [DiscoverStruct] -> [String]
leavesPaths ds = leavesPaths' "" ds
  where
    leavesPaths' :: String -> [DiscoverStruct] -> [String]
    leavesPaths' path ds = concat $ map (\str ->
                                           case str of
                                             Leaf name -> [path ++ name]
                                             Node name ds' -> leavesPaths' (path++name++".") ds') ds

