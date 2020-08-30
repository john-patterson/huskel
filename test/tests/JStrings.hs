module Tests.JStrings (suite) where
import Test.HUnit
import qualified John.TestInterop as Interop

import JStrings

testParseNumber_IsInteger :: Test
testParseNumber_IsInteger = TestCase (do
        (assertEqual "0 parsed" (parseNumber "0") (Just 0))
        (assertEqual "negative parsed" (parseNumber "-10") (Just $ -10))
        (assertEqual "positive parsed" (parseNumber "10") (Just 10))
        (assertEqual "bad string" (parseNumber "test") Nothing)
    )

suite = Interop.TestSuite {
    Interop.name = "JStrings.hs tests",
    Interop.tests = [testParseNumber_IsInteger]
}
