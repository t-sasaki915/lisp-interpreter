import LexicalScopeSpec
import LispMathsSpec
import LispSyntaxSpec
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

            , TestLabel "Lisp IF Test 1"                      lispIFTest1
            , TestLabel "Lisp IF Test 2"                      lispIFTest2
            , TestLabel "Lisp QUOTE Test 1"                   lispQUOTETest1
            , TestLabel "Lisp PROGN Test 1"                   lispPROGNTest1
            , TestLabel "Lisp PROGN Test 2"                   lispPROGNTest2
            , TestLabel "Lisp PROGN Test 3"                   lispPROGNTest3

            , TestLabel "Lexical Scope Test 1"                lexicalScopeTest1
            , TestLabel "Lexical Scope Test 2"                lexicalScopeTest2
            , TestLabel "Lexical Scope Test 3"                lexicalScopeTest3
            , TestLabel "Lexical Scope Test 4"                lexicalScopeTest4
            , TestLabel "Lexical Scope Test 5"                lexicalScopeTest5

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
            , TestLabel "Lisp Less Than Test 1"               lispLessThanTest1
            , TestLabel "Lisp Less Than Test 2"               lispLessThanTest2
            , TestLabel "Lisp Less Than Test 3"               lispLessThanTest3
            , TestLabel "Lisp Less Than Test 4"               lispLessThanTest4
            , TestLabel "Lisp Less Than Test 5"               lispLessThanTest5
            , TestLabel "Lisp Less Than Test 6"               lispLessThanTest6
            , TestLabel "Lisp Less Than Test 7"               lispLessThanTest7
            , TestLabel "Lisp Less Than Test 8"               lispLessThanTest8
            , TestLabel "Lisp Less Than Test 9"               lispLessThanTest9
            , TestLabel "Lisp Less Than Test 10"              lispLessThanTest10
            , TestLabel "Lisp Less Than Or Eq Test 1"         lispLessThanOrEqTest1
            , TestLabel "Lisp Less Than Or Eq Test 2"         lispLessThanOrEqTest2
            , TestLabel "Lisp Less Than Or Eq Test 3"         lispLessThanOrEqTest3
            , TestLabel "Lisp Less Than Or Eq Test 4"         lispLessThanOrEqTest4
            , TestLabel "Lisp Less Than Or Eq Test 5"         lispLessThanOrEqTest5
            , TestLabel "Lisp Less Than Or Eq Test 6"         lispLessThanOrEqTest6
            , TestLabel "Lisp Less Than Or Eq Test 7"         lispLessThanOrEqTest7
            , TestLabel "Lisp Less Than Or Eq Test 8"         lispLessThanOrEqTest8
            , TestLabel "Lisp Less Than Or Eq Test 9"         lispLessThanOrEqTest9
            , TestLabel "Lisp Less Than Or Eq Test 10"        lispLessThanOrEqTest10
            , TestLabel "Lisp Number Equal Test 1"            lispNumberEqTest1
            , TestLabel "Lisp Number Equal Test 2"            lispNumberEqTest2
            , TestLabel "Lisp Number Equal Test 3"            lispNumberEqTest3
            , TestLabel "Lisp Number Equal Test 4"            lispNumberEqTest4
            , TestLabel "Lisp Number Equal Test 5"            lispNumberEqTest5
            , TestLabel "Lisp Number Equal Test 6"            lispNumberEqTest6
            , TestLabel "Lisp Number Equal Test 7"            lispNumberEqTest7
            , TestLabel "Lisp Number Equal Test 8"            lispNumberEqTest8
            , TestLabel "Lisp Number Equal Test 9"            lispNumberEqTest9
            , TestLabel "Lisp Number Equal Test 10"           lispNumberEqTest10
            , TestLabel "Lisp Number Equal Test 11"           lispNumberEqTest11
            , TestLabel "Lisp Greater Than Test 1"            lispGreaterThanTest1
            , TestLabel "Lisp Greater Than Test 2"            lispGreaterThanTest2
            , TestLabel "Lisp Greater Than Test 3"            lispGreaterThanTest3
            , TestLabel "Lisp Greater Than Test 4"            lispGreaterThanTest4
            , TestLabel "Lisp Greater Than Test 5"            lispGreaterThanTest5
            , TestLabel "Lisp Greater Than Test 6"            lispGreaterThanTest6
            , TestLabel "Lisp Greater Than Test 7"            lispGreaterThanTest7
            , TestLabel "Lisp Greater Than Test 8"            lispGreaterThanTest8
            , TestLabel "Lisp Greater Than Test 9"            lispGreaterThanTest9
            , TestLabel "Lisp Greater Than Test 10"           lispGreaterThanTest10
            , TestLabel "Lisp Greater Than Or Eq Test 1"      lispGreaterThanOrEqTest1
            , TestLabel "Lisp Greater Than Or Eq Test 2"      lispGreaterThanOrEqTest2
            , TestLabel "Lisp Greater Than Or Eq Test 3"      lispGreaterThanOrEqTest3
            , TestLabel "Lisp Greater Than Or Eq Test 4"      lispGreaterThanOrEqTest4
            , TestLabel "Lisp Greater Than Or Eq Test 5"      lispGreaterThanOrEqTest5
            , TestLabel "Lisp Greater Than Or Eq Test 6"      lispGreaterThanOrEqTest6
            , TestLabel "Lisp Greater Than Or Eq Test 7"      lispGreaterThanOrEqTest7
            , TestLabel "Lisp Greater Than Or Eq Test 8"      lispGreaterThanOrEqTest8
            , TestLabel "Lisp Greater Than Or Eq Test 9"      lispGreaterThanOrEqTest9
            , TestLabel "Lisp Greater Than Or Eq Test 10"     lispGreaterThanOrEqTest10
            , TestLabel "Lisp ABS Test 1"                     lispABSTest1
            , TestLabel "Lisp ABS Test 2"                     lispABSTest2
            , TestLabel "Lisp ABS Test 3"                     lispABSTest3
            , TestLabel "Lisp COS Test 1"                     lispCOSTest1
            , TestLabel "Lisp COS Test 2"                     lispCOSTest2
            , TestLabel "Lisp EXPT Test 1"                    lispEXPTTest1
            , TestLabel "Lisp EXPT Test 2"                    lispEXPTTest2
            , TestLabel "Lisp EXPT Test 3"                    lispEXPTTest3
            , TestLabel "Lisp EXPT Test 4"                    lispEXPTTest4
            , TestLabel "Lisp EXPT Test 5"                    lispEXPTTest5
            , TestLabel "Lisp EXPT Test 6"                    lispEXPTTest6
            , TestLabel "Lisp EXPT Test 7"                    lispEXPTTest7
            , TestLabel "Lisp EXPT Test 8"                    lispEXPTTest8
            , TestLabel "Lisp EXPT Test 9"                    lispEXPTTest9
            , TestLabel "Lisp EXPT Test 10"                   lispEXPTTest10
            , TestLabel "Lisp EXPT Test 11"                   lispEXPTTest11
            , TestLabel "Lisp EXPT Test 12"                   lispEXPTTest12
            , TestLabel "Lisp EXPT Test 13"                   lispEXPTTest13
            , TestLabel "Lisp MAX Test 1"                     lispMAXTest1
            , TestLabel "Lisp MAX Test 2"                     lispMAXTest2
            , TestLabel "Lisp MIN Test 1"                     lispMINTest1
            , TestLabel "Lisp MIN Test 2"                     lispMINTest2
            , TestLabel "Lisp NOT Test 1"                     lispNOTTest1
            , TestLabel "Lisp NOT Test 2"                     lispNOTTest2
            , TestLabel "Lisp NOT Test 3"                     lispNOTTest3
            , TestLabel "Lisp SIN Test 1"                     lispSINTest1
            , TestLabel "Lisp SIN Test 2"                     lispSINTest2
            , TestLabel "Lisp SQRT Test 1"                    lispSQRTTest1
            , TestLabel "Lisp SQRT Test 2"                    lispSQRTTest2
            , TestLabel "Lisp SQRT Test 3"                    lispSQRTTest3
            , TestLabel "Lisp NUMBERP Test 1"                 lispNUMBERPTest1
            , TestLabel "Lisp NUMBERP Test 2"                 lispNUMBERPTest2
            ]
