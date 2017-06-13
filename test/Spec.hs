import           Test.HUnit

import           Data.List
import           Iec61850.BitString
import           Iec61850.NameTree

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
          , TestLabel "leavesPathsTest" leavesPathsTest
          , TestLabel "leavesPathsTest2" leavesPathsTest2
          , TestLabel "leavesPathsTest3" leavesPathsTest3                    
          ]

buildNameTree1 = TestCase $ assertEqual "" [] (buildNameTree [])

buildNameTree2 = TestCase $ assertEqual "" [Leaf "MX"] (buildNameTree ["MX"])

buildNameTree3 = TestCase $ assertEqual "" [Node "MX" [Leaf "A1"]] (buildNameTree ["MX", "MX$A1"])

buildNameTree4 = TestCase $ assertEqual "" [(Node "MX" [Leaf "A1"]), Leaf "CX"]
                              (buildNameTree ["MX", "MX$A1", "CX"])

leavesPathsTest = TestCase $ assertEqual "" ["A.B"] $ leavesPaths [Node "A" [Leaf "B"]]
leavesPathsTest2 = TestCase $ assertEqual "" ["C.D.E"] $ leavesPaths [Node "C" [Node "D" [ Leaf "E"]]]
leavesPathsTest3 = TestCase $ assertEqual "" ["A.B", "C.D.E"] $ leavesPaths [Node "A" [Leaf "B"], Node "C" [Node "D" [ Leaf "E"]]]

