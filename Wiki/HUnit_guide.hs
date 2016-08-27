-- |---------------------------------|--
-- |  DOCUMENTATION REGARDING HUNIT  |--
-- |---------------------------------|--

-- Purpose: Explain to the reader how to unit test in haskell using the HUnit framework.
-- Prereq: Basic Haskell programming skills assumed, Familiar with the concept of unit testing

-- Use cabal to install the HUnit package.

-- The HUnit testing framework exports a data type known as test which is defined as follows
-- data Test = TestCase Assertion | TestList [Test] | TestLabel String Test

-- Using the code below, the author will try explain to the reader how to create a 
-- unit test suite in haskell using the HUnit testing framework.

-- The HUnit framework exports a number of assertions which the programmer
-- can use to contstruct their test cases. For the purposes of this guide
-- I will only be using one of the many assertions found in the HUnit package in my examples
-- namely (assertEqual :: (Show a, Eq a) => String -> a -> a -> Assertion) 
-- Assertion has type IO ()

-- Now we have all the necessary tools to start writing our test cases. Lets go! :)

import Test.HUnit
import System.Exit

main :: IO ()
main = do
  results <- runTestTT testSuite2
  if (errors results + failures results == 0)
    then
      exitWith ExitSuccess
    else
      exitWith (ExitFailure 1)
-- I will be using the following simple function to illustrate how to build test cases
square :: (Num a) => a -> a
square x = x * x

-- To build a simple test case we can use the value constructor Test and the assertion assertEqual.
test1 = TestCase $ assertEqual "Name of assertion" 1 (square 1)
test2 = TestCase $ assertEqual "Assertion: Test 2" 4 (square 2)

-- load this file up in to a ghci interpreter
-- run the command runTestTT with any of the example tests cases
-- e.g runTestTT test1

-- run both of the tests and inspect the output, it will become clear what is happening!

-- As the number of tests you write increase, the progrrammer may like to group
-- similar tests into groups and run them simultaneously.

-- HUnit enables the user to group tests together using the value constructor TestList [Test], 
-- which takes as its arguments a list of tests. 
-- Lets take our previous tests and group them in to a list.

testGroup1 = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

-- Run runTestTT again on the intepreter but pass it the argument testGroup1.
-- All the tests in testGroup1 get run simultaneously.

-- To illustrate further the power of grouping and how we can run multiple groups of tests at the same time
-- let write a few more test cases.

test3 = TestCase $ [2] @=? (tail [1,2])
test4 = TestCase $ assertEqual "Assertion Test 4" 16 (square 5)

-- lets group these tests in to a group
testGroup2 = TestList [TestLabel "test3" test3, TestLabel "test4" test4]

-- A key feature of the TestList constructor is that is also of type Test so you 
-- can compose together groups of tests and run them at the same time.
-- Lets take our two groups of test and put them in to one big group

testSuite = TestList [TestLabel "testGroup1" testGroup1, TestLabel "testGroup2" testGroup2]

-- Run the testSuite using the test runner i.e runTestTT testSuite
-- I have pasted the output of the terminal below and highlighted the important parts.
-- sample output:
-- ### Failure in: 0:1  <<-- 1st group of tests in the list (this corresponds to testGroup1)                     
-- HUnit_guide.hs:38
-- Assertion: Test 2
-- expected: 2
-- but got: 4
-- ###Failure in: 1:1 <<-- 2nd group of tests in the list (this corresponds to testGroup2)                      
-- HUnit_guide.hs:64
-- Assertion Test 4
-- expected: 5
-- but got: 16


-- The HUnit framework, allows us to name tests so that we can produce more user-friendly output
-- lets do that!

testGroupLabel1 = TestLabel "label1" $ TestList [test1, test2]

testGroupLabel2 = TestLabel "label2" $ TestList [test3, test4]

testSuite2 = TestList [testGroupLabel1, testGroupLabel2]

-- now run testsuite2, inspect the output and you should see 
-- that the tests are grouped and titled with the appropriate labels.

-- This is a simple introduction on how to create unit tests using HUnit
-- There is a lot this guide has not covered e.g multiple assertions in one testcase, reporting etc
-- The HUnit user guide is a good place to start for 
-- further reading http://hunit.sourceforge.net/HUnit-1.0/Guide.html

-- hope this guide was helpful :)

-- we can 
reportMsg :: String -> Bool -> Int -> IO Int
reportMsg message isProgress count = do
  let x = if isProgress then "yay" else "nay"
  putStrLn x
  putStrLn $ "#" ++ show (count+1) ++ ": " ++ message
  return (count+1)

myPutText = PutText reportMsg 0  :: PutText Int


