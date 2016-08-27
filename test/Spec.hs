import Test.HUnit
import System.Exit
main :: IO ()
main = do
  results <- runTestTT test1
  if errors results + failures results == 0
    then
      exitSuccess
    else
      exitFailure






test1 = TestCase $ assertEqual "test" 2 2
test2 = TestCase $ assertEqual "test" 2 2

testGroup1 = TestList [TestLabel "test1" test1, TestLabel "test2" test2]
