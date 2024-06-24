{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispMathsSpec where

import LispSpec
import LispMaths

import Data.Ratio ((%))
import Test.HUnit (Test)

lispMultipleTest1 :: Test
lispMultipleTest1 = lispProcedureTest lispMultiple
    [integer 1, integer 2, integer 3]
    (return (integer 6))

lispMultipleTest2 :: Test
lispMultipleTest2 = lispProcedureTest lispMultiple
    [integer 1, integer 2]
    (return (integer 2))

lispMultipleTest3 :: Test
lispMultipleTest3 = lispProcedureTest lispMultiple
    [integer 1]
    (return (integer 1))

lispMultipleTest4 :: Test
lispMultipleTest4 = lispProcedureTest lispMultiple
    []
    (return (integer 1))

lispMultipleTest5 :: Test
lispMultipleTest5 = lispProcedureTest lispMultiple
    [integer 1234567890123456789, integer 9876543210987654321]
    (return (integer 12193263113702179522374638011112635269))

lispMultipleTest6 :: Test
lispMultipleTest6 = lispProcedureTest lispMultiple
    [real 1.3, integer (-5)]
    (return (real (-6.5)))

lispMultipleTest7 :: Test
lispMultipleTest7 = lispProcedureTest lispMultiple
    [rational (3 % 4), rational (7 % 9)]
    (return (rational (7 % 12)))

lispAdditionTest1 :: Test
lispAdditionTest1 = lispProcedureTest lispAddition
    [integer 1, integer 2, integer 3]
    (return (integer 6))

lispAdditionTest2 :: Test
lispAdditionTest2 = lispProcedureTest lispAddition
    [integer 1, integer 2]
    (return (integer 3))

lispAdditionTest3 :: Test
lispAdditionTest3 = lispProcedureTest lispAddition
    [integer 1]
    (return (integer 1))

lispAdditionTest4 :: Test
lispAdditionTest4 = lispProcedureTest lispAddition
    []
    (return (integer 0))

lispAdditionTest5 :: Test
lispAdditionTest5 = lispProcedureTest lispAddition
    [integer 1234567890123456789, integer 9876543210987654321]
    (return (integer 11111111101111111110))

lispAdditionTest6 :: Test
lispAdditionTest6 = lispProcedureTest lispAddition
    [real 1.3, integer (-5)]
    (return (real (-3.7)))

lispAdditionTest7 :: Test
lispAdditionTest7 = lispProcedureTest lispAddition
    [rational (3 % 4), rational (7 % 9)]
    (return (rational (55 % 36)))

lispSubtractTest1 :: Test
lispSubtractTest1 = lispProcedureTest lispSubtract
    [integer 1, integer 2, integer 3]
    (return (integer (-4)))

lispSubtractTest2 :: Test
lispSubtractTest2 = lispProcedureTest lispSubtract
    [integer 1, integer 2]
    (return (integer (-1)))

lispSubtractTest3 :: Test
lispSubtractTest3 = lispProcedureTest lispSubtract
    [integer 1]
    (return (integer (-1)))

lispSubtractTest4 :: Test
lispSubtractTest4 = lispProcedureTest lispSubtract
    [integer 1234567890123456789, integer 9876543210987654321]
    (return (integer (-8641975320864197532)))

lispSubtractTest5 :: Test
lispSubtractTest5 = lispProcedureTest lispSubtract
    [real 1.3, integer (-5)]
    (return (real 6.3))

lispSubtractTest6 :: Test
lispSubtractTest6 = lispProcedureTest lispSubtract
    [rational (3 % 4), rational (7 % 9)]
    (return (rational (- (1 % 36))))

lispDivisionTest1 :: Test
lispDivisionTest1 = lispProcedureTest lispDivision
    [integer 10]
    (return (rational (1 % 10)))

lispDivisionTest2 :: Test
lispDivisionTest2 = lispProcedureTest lispDivision
    [real 10.0]
    (return (real 0.1))

lispDivisionTest3 :: Test
lispDivisionTest3 = lispProcedureTest lispDivision
    [integer 10, integer 2]
    (return (integer 5))

lispDivisionTest4 :: Test
lispDivisionTest4 = lispProcedureTest lispDivision
    [integer 2, integer 10]
    (return (rational (1 % 5)))

lispDivisionTest5 :: Test
lispDivisionTest5 = lispProcedureTest lispDivision
    [integer 100, integer 2, integer 5, integer 2]
    (return (integer 5))

lispDivisionTest6 :: Test
lispDivisionTest6 = lispProcedureTest lispDivision
    [integer 1234567890123456789, integer 9876543210987654321]
    (return (rational (13717421 % 109739369)))

lispDivisionTest7 :: Test
lispDivisionTest7 = lispProcedureTest lispDivision
    [real 1.3, integer (-5)]
    (return (real (-0.26)))

lispDivisionTest8 :: Test
lispDivisionTest8 = lispProcedureTest lispDivision
    [rational (3 % 4), rational (7 % 9)]
    (return (rational (27 % 28)))

lispLessThanTest1 :: Test
lispLessThanTest1 = lispProcedureTest lispLessThan
    [integer 1, integer 2]
    (return (boolean True))

lispLessThanTest2 :: Test
lispLessThanTest2 = lispProcedureTest lispLessThan
    [integer 2, integer 1]
    (return (boolean False))

lispLessThanTest3 :: Test
lispLessThanTest3 = lispProcedureTest lispLessThan
    [integer 2, real 2.001]
    (return (boolean True))

lispLessThanTest4 :: Test
lispLessThanTest4 = lispProcedureTest lispLessThan
    [integer 2, integer 2]
    (return (boolean False))

lispLessThanTest5 :: Test
lispLessThanTest5 = lispProcedureTest lispLessThan
    [integer 1234567890123456789, integer 9876543210987654321]
    (return (boolean True))

lispLessThanTest6 :: Test
lispLessThanTest6 = lispProcedureTest lispLessThan
    [integer 1, integer 2, integer 3, integer 4, integer 5]
    (return (boolean True))

lispLessThanTest7 :: Test
lispLessThanTest7 = lispProcedureTest lispLessThan
    [integer 1, integer 2, integer 4, integer 3, integer 5]
    (return (boolean False))

lispLessThanTest8 :: Test
lispLessThanTest8 = lispProcedureTest lispLessThan
    [integer 1, integer 2, integer 4, integer 4, integer 5]
    (return (boolean False))

lispLessThanTest9 :: Test
lispLessThanTest9 = lispProcedureTest lispLessThan
    [rational (3 % 4), rational (7 % 9)]
    (return (boolean True))

lispLessThanTest10 :: Test
lispLessThanTest10 = lispProcedureTest lispLessThan
    [integer 5]
    (return (boolean True))

lispLessThanOrEqTest1 :: Test
lispLessThanOrEqTest1 = lispProcedureTest lispLessThanOrEq
    [integer 1, integer 2]
    (return (boolean True))

lispLessThanOrEqTest2 :: Test
lispLessThanOrEqTest2 = lispProcedureTest lispLessThanOrEq
    [integer 2, integer 1]
    (return (boolean False))

lispLessThanOrEqTest3 :: Test
lispLessThanOrEqTest3 = lispProcedureTest lispLessThanOrEq
    [integer 2, real 2.001]
    (return (boolean True))

lispLessThanOrEqTest4 :: Test
lispLessThanOrEqTest4 = lispProcedureTest lispLessThanOrEq
    [integer 2, integer 2]
    (return (boolean True))

lispLessThanOrEqTest5 :: Test
lispLessThanOrEqTest5 = lispProcedureTest lispLessThanOrEq
    [integer 1234567890123456789, integer 9876543210987654321]
    (return (boolean True))

lispLessThanOrEqTest6 :: Test
lispLessThanOrEqTest6 = lispProcedureTest lispLessThanOrEq
    [integer 1, integer 2, integer 3, integer 4, integer 5]
    (return (boolean True))

lispLessThanOrEqTest7 :: Test
lispLessThanOrEqTest7 = lispProcedureTest lispLessThanOrEq
    [integer 1, integer 2, integer 4, integer 3, integer 5]
    (return (boolean False))

lispLessThanOrEqTest8 :: Test
lispLessThanOrEqTest8 = lispProcedureTest lispLessThanOrEq
    [integer 1, integer 2, integer 4, integer 4, integer 5]
    (return (boolean True))

lispLessThanOrEqTest9 :: Test
lispLessThanOrEqTest9 = lispProcedureTest lispLessThanOrEq
    [rational (3 % 4), rational (7 % 9)]
    (return (boolean True))

lispLessThanOrEqTest10 :: Test
lispLessThanOrEqTest10 = lispProcedureTest lispLessThanOrEq
    [integer 5]
    (return (boolean True))

lispNumberEqTest1 :: Test
lispNumberEqTest1 = lispProcedureTest lispNumberEq
    [integer 1, integer 2]
    (return (boolean False))

lispNumberEqTest2 :: Test
lispNumberEqTest2 = lispProcedureTest lispNumberEq
    [integer 2, integer 1]
    (return (boolean False))

lispNumberEqTest3 :: Test
lispNumberEqTest3 = lispProcedureTest lispNumberEq
    [integer 2, real 2.001]
    (return (boolean False))

lispNumberEqTest4 :: Test
lispNumberEqTest4 = lispProcedureTest lispNumberEq
    [integer 2, integer 2]
    (return (boolean True))

lispNumberEqTest5 :: Test
lispNumberEqTest5 = lispProcedureTest lispNumberEq
    [integer 2, real 2.0]
    (return (boolean True))

lispNumberEqTest6 :: Test
lispNumberEqTest6 = lispProcedureTest lispNumberEq
    [real 0.0, real (-0.0)]
    (return (boolean True))

lispNumberEqTest7 :: Test
lispNumberEqTest7 = lispProcedureTest lispNumberEq
    [integer 1, integer 2, integer 3, integer 4, integer 5]
    (return (boolean False))

lispNumberEqTest8 :: Test
lispNumberEqTest8 = lispProcedureTest lispNumberEq
    [integer 4, integer 4, integer 4, integer 3, integer 4]
    (return (boolean False))

lispNumberEqTest9 :: Test
lispNumberEqTest9 = lispProcedureTest lispNumberEq
    [integer 4, integer 4, integer 4, integer 4, integer 4]
    (return (boolean True))

lispNumberEqTest10 :: Test
lispNumberEqTest10 = lispProcedureTest lispNumberEq
    [integer 4, integer 4, integer 4, real 4.0, integer 4]
    (return (boolean True))

lispNumberEqTest11 :: Test
lispNumberEqTest11 = lispProcedureTest lispNumberEq
    [integer 5]
    (return (boolean True))

lispGreaterThanTest1 :: Test
lispGreaterThanTest1 = lispProcedureTest lispGreaterThan
    [integer 2, integer 1]
    (return (boolean True))

lispGreaterThanTest2 :: Test
lispGreaterThanTest2 = lispProcedureTest lispGreaterThan
    [integer 1, integer 2]
    (return (boolean False))

lispGreaterThanTest3 :: Test
lispGreaterThanTest3 = lispProcedureTest lispGreaterThan
    [real 2.001, integer 2]
    (return (boolean True))

lispGreaterThanTest4 :: Test
lispGreaterThanTest4 = lispProcedureTest lispGreaterThan
    [integer 2, integer 2]
    (return (boolean False))

lispGreaterThanTest5 :: Test
lispGreaterThanTest5 = lispProcedureTest lispGreaterThan
    [integer 9876543210987654321, integer 1234567890123456789]
    (return (boolean True))

lispGreaterThanTest6 :: Test
lispGreaterThanTest6 = lispProcedureTest lispGreaterThan
    [integer 5, integer 4, integer 3, integer 2, integer 1]
    (return (boolean True))

lispGreaterThanTest7 :: Test
lispGreaterThanTest7 = lispProcedureTest lispGreaterThan
    [integer 5, integer 3, integer 4, integer 2, integer 1]
    (return (boolean False))

lispGreaterThanTest8 :: Test
lispGreaterThanTest8 = lispProcedureTest lispGreaterThan
    [integer 5, integer 4, integer 4, integer 2, integer 1]
    (return (boolean False))

lispGreaterThanTest9 :: Test
lispGreaterThanTest9 = lispProcedureTest lispGreaterThan
    [rational (7 % 9), rational (3 % 4)]
    (return (boolean True))

lispGreaterThanTest10 :: Test
lispGreaterThanTest10 = lispProcedureTest lispGreaterThan
    [integer 5]
    (return (boolean True))

lispGreaterThanOrEqTest1 :: Test
lispGreaterThanOrEqTest1 = lispProcedureTest lispGreaterThanOrEq
    [integer 2, integer 1]
    (return (boolean True))

lispGreaterThanOrEqTest2 :: Test
lispGreaterThanOrEqTest2 = lispProcedureTest lispGreaterThanOrEq
    [integer 1, integer 2]
    (return (boolean False))

lispGreaterThanOrEqTest3 :: Test
lispGreaterThanOrEqTest3 = lispProcedureTest lispGreaterThanOrEq
    [real 2.001, integer 2]
    (return (boolean True))

lispGreaterThanOrEqTest4 :: Test
lispGreaterThanOrEqTest4 = lispProcedureTest lispGreaterThanOrEq
    [integer 2, integer 2]
    (return (boolean True))

lispGreaterThanOrEqTest5 :: Test
lispGreaterThanOrEqTest5 = lispProcedureTest lispGreaterThanOrEq
    [integer 9876543210987654321, integer 1234567890123456789]
    (return (boolean True))

lispGreaterThanOrEqTest6 :: Test
lispGreaterThanOrEqTest6 = lispProcedureTest lispGreaterThanOrEq
    [integer 5, integer 4, integer 3, integer 2, integer 1]
    (return (boolean True))

lispGreaterThanOrEqTest7 :: Test
lispGreaterThanOrEqTest7 = lispProcedureTest lispGreaterThanOrEq
    [integer 5, integer 3, integer 4, integer 2, integer 1]
    (return (boolean False))

lispGreaterThanOrEqTest8 :: Test
lispGreaterThanOrEqTest8 = lispProcedureTest lispGreaterThanOrEq
    [integer 5, integer 4, integer 4, integer 2, integer 1]
    (return (boolean True))

lispGreaterThanOrEqTest9 :: Test
lispGreaterThanOrEqTest9 = lispProcedureTest lispGreaterThanOrEq
    [rational (7 % 9), rational (3 % 4)]
    (return (boolean True))

lispGreaterThanOrEqTest10 :: Test
lispGreaterThanOrEqTest10 = lispProcedureTest lispGreaterThanOrEq
    [integer 5]
    (return (boolean True))

lispABSTest1 :: Test
lispABSTest1 = lispProcedureTest lispABS
    [integer 0]
    (return (integer 0))

lispABSTest2 :: Test
lispABSTest2 = lispProcedureTest lispABS
    [rational (12 % 13)]
    (return (rational (12 % 13)))

lispABSTest3 :: Test
lispABSTest3 = lispProcedureTest lispABS
    [real (-1.09)]
    (return (real 1.09))

lispCOSTest1 :: Test
lispCOSTest1 = lispProcedureTest lispCOS
    [real 0.0]
    (return (real 1.0))

lispCOSTest2 :: Test
lispCOSTest2 = lispProcedureTest lispCOS
    [real 1.0]
    (return (real 0.5403023))

lispEXPTTest1 :: Test
lispEXPTTest1 = lispProcedureTest lispEXPT
    [integer 2, integer 8]
    (return (integer 256))

lispEXPTTest2 :: Test
lispEXPTTest2 = lispProcedureTest lispEXPT
    [integer 2, integer 32]
    (return (integer 4294967296))

lispEXPTTest3 :: Test
lispEXPTTest3 = lispProcedureTest lispEXPT
    [integer 2, integer 64]
    (return (integer 18446744073709551616))

lispEXPTTest4 :: Test
lispEXPTTest4 = lispProcedureTest lispEXPT
    [integer 10, integer 3]
    (return (integer 1000))

lispEXPTTest5 :: Test
lispEXPTTest5 = lispProcedureTest lispEXPT
    [integer 5, rational (1 % 3)]
    (return (real 1.709976))

lispEXPTTest6 :: Test
lispEXPTTest6 = lispProcedureTest lispEXPT
    [real 1.709976, integer 3]
    (return (real 5.0))

lispEXPTTest7 :: Test
lispEXPTTest7 = lispProcedureTest lispEXPT
    [integer 1, integer 2]
    (return (integer 1))

lispEXPTTest8 :: Test
lispEXPTTest8 = lispProcedureTest lispEXPT
    [real 1.0, integer 2]
    (return (real 1.0))

lispEXPTTest9 :: Test
lispEXPTTest9 = lispProcedureTest lispEXPT
    [integer (-2), integer 5]
    (return (integer (-32)))

lispEXPTTest10 :: Test
lispEXPTTest10 = lispProcedureTest lispEXPT
    [integer 2, real 2.5]
    (return (real 5.656854))

lispEXPTTest11 :: Test
lispEXPTTest11 = lispProcedureTest lispEXPT
    [integer 2, integer (-3)]
    (return (rational (1 % 8)))

lispEXPTTest12 :: Test
lispEXPTTest12 = lispProcedureTest lispEXPT
    [integer (-2), integer (-3)]
    (return (rational ((-1) % 8)))

lispEXPTTest13 :: Test
lispEXPTTest13 = lispProcedureTest lispEXPT
    [real (-2.4), integer (-3)]
    (return (real (-0.072337955)))

lispMAXTest1 :: Test
lispMAXTest1 = lispProcedureTest lispMAX
    [integer 1, integer 3, integer 2]
    (return (integer 3))

lispMAXTest2 :: Test
lispMAXTest2 = lispProcedureTest lispMAX
    [integer 4]
    (return (integer 4))

lispMINTest1 :: Test
lispMINTest1 = lispProcedureTest lispMIN
    [integer 1, integer 3, integer 2]
    (return (integer 1))

lispMINTest2 :: Test
lispMINTest2 = lispProcedureTest lispMIN
    [integer 4]
    (return (integer 4))

lispNOTTest1 :: Test
lispNOTTest1 = lispProcedureTest lispNOT
    [boolean True]
    (return (boolean False))

lispNOTTest2 :: Test
lispNOTTest2 = lispProcedureTest lispNOT
    [boolean False]
    (return (boolean True))

lispNOTTest3 :: Test
lispNOTTest3 = lispProcedureTest lispNOT
    [real 234.3]
    (return (boolean False))

lispSINTest1 :: Test
lispSINTest1 = lispProcedureTest lispSIN
    [real 0.0]
    (return (real 0.0))

lispSINTest2 :: Test
lispSINTest2 = lispProcedureTest lispSIN
    [real 1.0]
    (return (real 0.84147096))

lispSQRTTest1 :: Test
lispSQRTTest1 = lispProcedureTest lispSQRT
    [integer 10]
    (return (real 3.1622777))

lispSQRTTest2 :: Test
lispSQRTTest2 = lispProcedureTest lispSQRT
    [real 10.0]
    (return (real 3.1622777))

lispSQRTTest3 :: Test
lispSQRTTest3 = lispProcedureTest lispSQRT
    [integer 4]
    (return (real 2.0))

lispNUMBERPTest1 :: Test
lispNUMBERPTest1 = lispProcedureTest lispNUMBERP
    [integer 12]
    (return (boolean True))

lispNUMBERPTest2 :: Test
lispNUMBERPTest2 = lispProcedureTest lispNUMBERP
    [boolean False]
    (return (boolean False))
