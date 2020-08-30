module MainSuite (fullSuite) where
import John.TestInterop
import Tests.Compiler as Compiler
import Tests.JStrings as JStrings


fullSuite :: [TestSuite]
fullSuite = [
        Compiler.suite,
        JStrings.suite
    ]