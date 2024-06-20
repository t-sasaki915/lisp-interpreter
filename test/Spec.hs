import ParserSpec

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 || errors result > 0 then
        exitFailure

    else
        exitSuccess
    where
        tests = TestList
            [ TestLabel "Simple List Parsing"   parserTest1
            , TestLabel "Multiple List Parsing" parserTest2
            , TestLabel "Type Recognition"      parserTest3
            , TestLabel "Nested List Parsing"   parserTest4
            , TestLabel "Comment Parsing"       parserTest5
            ]
