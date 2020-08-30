module MainSuite (fullSuite) where
import John.TestInterop
import Tests.Main as Main
import Tests.JStrings as JStrings


fullSuite :: [TestSuite]
fullSuite = [
        Main.suite,
        JStrings.suite
    ]