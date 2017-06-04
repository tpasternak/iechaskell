import           Test.HUnit

import           BitString

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

tests = TestList [TestLabel "testShow" testShow, TestLabel "testToList" testToList]
