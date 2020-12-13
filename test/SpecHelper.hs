module SpecHelper
  ( module Test.Hspec,
    TestCase (..),
    runTestCase,
    runTestCases,
  )
where

import Data.Foldable
import Test.Hspec

data TestCase = TestCase String Int

runTestCase :: (String -> Int) -> TestCase -> IO ()
runTestCase function (TestCase input expected) = do
  contents <- readFile input
  function contents `shouldBe` expected

runTestCases :: (String -> Int) -> [TestCase] -> IO ()
runTestCases function cases = for_ cases test
  where
    test testCase = runTestCase function testCase