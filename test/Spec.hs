import EvalSpec
import LispMathsSpec
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
            [ TestLabel "Simple List Parsing"                 parserTest1
            , TestLabel "Multiple List Parsing"               parserTest2
            , TestLabel "Type Recognition"                    parserTest3
            , TestLabel "Nested List Parsing"                 parserTest4
            , TestLabel "Comment Parsing"                     parserTest5
            , TestLabel "Quote Parsing"                       parserTest6

            , TestLabel "Simple If Evaluation"                evalTest1
            , TestLabel "Nested If Evaluation"                evalTest2
            , TestLabel "Else Case Reduced If Evaluation"     evalTest3
            , TestLabel "Too Few Arguments If Evaluation"     evalTest4
            , TestLabel "Too Many Arguments If Evaluation"    evalTest5
            , TestLabel "Quote Evaluation"                    evalTest6
            , TestLabel "Nested Quote Evaluation"             evalTest7
            , TestLabel "Too Many Arguments Quote Evaluation" evalTest8
            , TestLabel "Too Few Arguments Quote Evaluation"  evalTest9

            , TestLabel "Lisp Multiplication Test 1"          lispMultipleTest1
            , TestLabel "Lisp Multiplication Test 2"          lispMultipleTest2
            , TestLabel "Lisp Multiplication Test 3"          lispMultipleTest3
            , TestLabel "Lisp Multiplication Test 4"          lispMultipleTest4
            , TestLabel "Lisp Multiplication Test 5"          lispMultipleTest5
            , TestLabel "Lisp Multiplication Test 6"          lispMultipleTest6
            , TestLabel "Lisp Multiplication Test 7"          lispMultipleTest7
            , TestLabel "Lisp Addition Test 1"                lispAdditionTest1
            , TestLabel "Lisp Addition Test 2"                lispAdditionTest2
            , TestLabel "Lisp Addition Test 3"                lispAdditionTest3
            , TestLabel "Lisp Addition Test 4"                lispAdditionTest4
            , TestLabel "Lisp Addition Test 5"                lispAdditionTest5
            , TestLabel "Lisp Addition Test 6"                lispAdditionTest6
            , TestLabel "Lisp Addition Test 7"                lispAdditionTest7
            , TestLabel "Lisp Subtract Test 1"                lispSubtractTest1
            , TestLabel "Lisp Subtract Test 2"                lispSubtractTest2
            , TestLabel "Lisp Subtract Test 3"                lispSubtractTest3
            , TestLabel "Lisp Subtract Test 4"                lispSubtractTest4
            , TestLabel "Lisp Subtract Test 5"                lispSubtractTest5
            , TestLabel "Lisp Subtract Test 6"                lispSubtractTest6
            , TestLabel "Lisp Division Test 1"                lispDivisionTest1
            , TestLabel "Lisp Division Test 2"                lispDivisionTest2
            , TestLabel "Lisp Division Test 3"                lispDivisionTest3
            , TestLabel "Lisp Division Test 4"                lispDivisionTest4
            , TestLabel "Lisp Division Test 5"                lispDivisionTest5
            , TestLabel "Lisp Division Test 6"                lispDivisionTest6
            , TestLabel "Lisp Division Test 7"                lispDivisionTest7
            , TestLabel "Lisp Division Test 8"                lispDivisionTest8
            ]
