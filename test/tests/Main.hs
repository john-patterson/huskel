module Tests.Main (suite) where
import Test.HUnit
import qualified John.TestInterop as Interop
import Compiler

samplePass :: Test
samplePass = TestCase (
        assertEqual "add identity pass" (performOperation Add 0 5) 5
    )

suite = Interop.TestSuite {
    Interop.name = "Main.hs tests",
    Interop.tests = [samplePass]
}