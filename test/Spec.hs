import           Test.HUnit

import           Data.List
import           Data.List.Split
import           Iec61850.BitString

main = runTestTT tests

-- bitStringToList bs = undefined
testToList :: Test
testToList =
  let expected = map (== 2) [0 .. 31]
      actual = bitStringToList $ BitString 4
  in TestCase $ assertEqual "" expected actual

testShow :: Test
testShow =
  let expected = "00100000" ++ "00000000" ++ "00000000" ++ "00000000"
      actual = show $ BitString 4
  in TestCase $ assertEqual "" expected actual

tests = TestList
          [ TestLabel "testShow" testShow
          , TestLabel "testToList" testToList
          , TestLabel "buildNameTree1" buildNameTree1
          , TestLabel "buildNameTree2" buildNameTree2
          , TestLabel "buildNameTree3" buildNameTree3
          , TestLabel "buildNameTree4" buildNameTree4
          ]

buildNameTree1 = TestCase $ assertEqual "" [] (buildNameTree [])

buildNameTree2 = TestCase $ assertEqual "" [Leaf "MX"] (buildNameTree ["MX"])

buildNameTree3 = TestCase $ assertEqual "" [Node "MX" [Leaf "A1"]] (buildNameTree ["MX", "MX$A1"])

buildNameTree4 = TestCase $ assertEqual "" [(Node "MX" [Leaf "A1"]), Leaf "CX"]
                              (buildNameTree ["MX", "MX$A1", "CX"])

data DiscoverStruct = Leaf String
                    | Node String [DiscoverStruct]
  deriving (Eq, Show)

buildNameTreeH :: [[String]] -> [DiscoverStruct]
buildNameTreeH split_ =
  let roots = concat $ filter ((== 1) . length) split_
      newTrees = map
                   (\root -> let nextString = filter (\tree -> length tree >= 1 && root == head tree) split_
                                 asn = if length nextString == 1
                                         then Leaf root
                                         else Node root (buildNameTreeH (drop 1 <$> nextString))
                             in asn)
                   roots
  in newTrees

buildNameTree :: [String] -> [DiscoverStruct]
buildNameTree strings =
  let split_ = map (splitOn "$") strings
      s = buildNameTreeH split_
  in s--
      -- ["ST$Proxy$t","ST$Proxy$stVal","ST$Proxy$q","ST$Proxy","ST$PhyHealth$t","ST$PhyHealth$stVal","ST$PhyHealth$q","ST$PhyHealth","ST","DC$PhyNam$vendor","DC$PhyNam","DC"]
