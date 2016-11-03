import Test.HUnit
import System.Exit
import Utility.Definitions
import Utility.BasicCombinators
import Utility.Declarations
import Parser
main :: IO ()
main = do
  results <- runTestTT allTests
  if errors results + failures results == 0
    then
      putStrLn "All passed"
    else
      putStrLn "Something failed"

allTests = TestList [testIntLiteralGroup, testBoolLiteralGroup, testCharLiteralGroup, testPairLiteralGroup]

-- Test Groups
testIntLiteralGroup = TestLabel "IntLiteralTests" $ TestList [test_intLit1, test_intLit2, test_intLit3, test_intLit4]
testBoolLiteralGroup = TestLabel "BoolLiteralTests" $ TestList [test_boolLit1, test_boolLit2, test_boolLit3]
testCharLiteralGroup = TestLabel "CharLiteralTests" $ TestList [test_charLit1, test_charLit2, test_charLit3, test_charLit4, test_charLit5, test_charLit6, test_charLit7, test_charLit8, test_charLit9, test_charLit10,
                                                                test_charLit11, test_charLit12]
testPairLiteralGroup = TestLabel "PairLiteralTests" $ TestList [test_pairLit1, test_pairLit2, test_pairLit3, test_pairLit4, test_pairLit5]                                                               

-- Tests for intLiteral
test_intLit1 = TestCase $ assertEqual "Parse (single digit)" [(IntLit 5, "")] (parse intLiteral "5")
test_intLit2 = TestCase $ assertEqual "Parse (multi digit)" [(IntLit 512312, "")] (parse intLiteral "512312")
test_intLit3 = TestCase $ assertEqual "Parse only digits" [(IntLit 512312, "dontparsethis")] (parse intLiteral "512312dontparsethis")
test_intLit4 = TestCase $ assertEqual "Parse check if fails" [] (parse intLiteral "_12") 

-- Tests for booLiteral.
test_boolLit1 = TestCase $ assertEqual "Parse True" [(BoolLit True, "")] (parse boolLiteral "true")
test_boolLit2 = TestCase $ assertEqual "Parse False" [(BoolLit False, "")] (parse boolLiteral "false")
test_boolLit3 = TestCase $ assertEqual "Parse check if fails" [] (parse boolLiteral "trueshouldfail") --- interesting test case??

-- Tests for charLiteral
test_charLit1  = TestCase $ assertEqual "Parse one char" [(CharLit 'p',"arser")] (parse charLiteral "\'p\'arser")
test_charLit2  = TestCase $ assertEqual "Parse escape char \\b" [(CharLit '\b', "rest")] (parse charLiteral "\'\\b\'rest")	
test_charLit3  = TestCase $ assertEqual "Parse escape char \\n" [(CharLit '\n', "rest")] (parse charLiteral "\'\\n\'rest")
test_charLit4  = TestCase $ assertEqual "Parse escape char \\f" [(CharLit '\f', "rest")] (parse charLiteral "\'\\f\'rest")
test_charLit5  = TestCase $ assertEqual "Parse escape char \\r" [(CharLit '\r', "rest")] (parse charLiteral "\'\\r\'rest")
test_charLit6  = TestCase $ assertEqual "Parse escape char \\t" [(CharLit '\t', "rest")] (parse charLiteral "\'\\t\'rest")
test_charLit7  = TestCase $ assertEqual "Parse escape char \\" [(CharLit '\\', "rest")] (parse charLiteral "\'\\\\'rest")
test_charLit8  = TestCase $ assertEqual "Parse escape char \"" [(CharLit '\"', "rest")] (parse charLiteral "\'\\\"\'rest")
test_charLit9  = TestCase $ assertEqual "Parse escape char \'" [(CharLit '\'', "rest")] (parse charLiteral "\'\\'\'rest")
test_charLit10 = TestCase $ assertEqual "Parse escape char \\" [(CharLit '0', "rest")] (parse charLiteral "\'\\0\'rest") -- interesting test case? do we want \NUl or \0
test_charLit11 = TestCase $ assertEqual "Parse check if fails" [] (parse charLiteral "shouldfail")
test_charLit12 = TestCase $ assertEqual "Parse check if fails" [] (parse charLiteral "\'thissouldfail")

-- Tests for pairLiteral
test_pairLit1 = TestCase $ assertEqual "Parse a pairLiteral" [(PairLiteral, "rest")] (parse pairLiteral "nullrest") -- this one or the one after?
test_pairLit2 = TestCase $ assertEqual "Parse a pairLiteral" [] (parse pairLiteral "nullrest") -- what behavior do we want?
test_pairLit3 = TestCase $ assertEqual "Parse a pairLiteral" [(PairLiteral, " rest")] (parse pairLiteral "null rest")
test_pairLit4 = TestCase $ assertEqual "Parse check if fails pairLit" [] (parse pairLiteral "gnullrest")
test_pairLit5 = TestCase $ assertEqual "Parse check if fails pairLit" [] (parse pairLiteral "nu llrest")

