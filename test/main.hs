module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import qualified John.TestInterop as Interop
import qualified MainSuite

testGroups :: [Test]
testGroups = map Interop.suiteAsTestGroup MainSuite.fullSuite
main :: IO ()
main = defaultMain testGroups