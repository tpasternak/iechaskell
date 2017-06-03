import Test.HUnit

import BitString

main = runTestTT tests

testFoo:: Test
testFoo = TestCase $ assertEqual "" 1 2

tests = TestList [ TestLabel "testFoo" testFoo ]
