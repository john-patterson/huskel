module MainSuite (fullSuite) where
import John.TestInterop
import Tests.Main as Main


fullSuite :: [TestSuite]
fullSuite = [
        Main.suite
    ]