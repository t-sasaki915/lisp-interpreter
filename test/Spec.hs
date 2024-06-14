import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

import SyntaxAnalyserTest
import TokeniserTest

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 || errors result > 0 then
        exitFailure

    else
        exitSuccess
    where
        tests = TestList
            [ TestLabel "Simple Lisp Source Tokenisation"        tokeniserTest1
            , TestLabel "Complex Lisp Source Tokenisation"       tokeniserTest2
            , TestLabel "Invalid String Format Tokenisation"     tokeniserTest3
            , TestLabel "Invalid Number Format Tokenisation"     tokeniserTest4

            , TestLabel "Simple Lisp Source Syntax Analysation"  syntaxAnalyserTest1
            , TestLabel "Nested List Analysation"                syntaxAnalyserTest2
            , TestLabel "Complex Lisp Source Syntax Analysation" syntaxAnalyserTest3
            , TestLabel "Invalid Close Parentheses Analysation"  syntaxAnalyserTest4
            , TestLabel "Missing Close Parentheses Analysation"  syntaxAnalyserTest5
            ]
