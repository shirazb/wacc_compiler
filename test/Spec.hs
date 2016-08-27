import Test.HUnit
import System.Exit
main :: IO ()
main = do
  results <- runTestTT test1
  if (errors results + failures results == 0)
    then
      exitWith ExitSuccess
    else
      exitWith (ExitFailure 1)



test1 = TestCase $ assertEqual "test" 2 2
test2 = TestCase $ assertEqual "test" 2 2
