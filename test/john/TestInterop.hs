module John.TestInterop (TestSuite (..), suiteAsTestGroup) where
import qualified Test.HUnit as HUnit
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)

data TestSuite = TestSuite {
    name :: String,
    tests :: [HUnit.Test]
}

suiteAsTestGroup :: TestSuite -> Test
suiteAsTestGroup suite = testGroup (name suite) (hUnitTestToTests $ HUnit.TestList (tests suite))