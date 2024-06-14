import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

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
            [ TestLabel "Simple Lisp Source Tokenisation"    tokeniserTest1
            , TestLabel "Complex Lisp Source Tokenisation"   tokeniserTest2
            , TestLabel "Invalid String Format Tokenisation" tokeniserTest3
            , TestLabel "Invalid Number Format Tokenisation" tokeniserTest4
            ]
