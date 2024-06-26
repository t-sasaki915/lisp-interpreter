{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispMathsSpec where

import LispSpec (lispProcedureTest)
import LispSystem (LispData(..))
import LispMaths

import Data.Ratio ((%))
import Test.HUnit (Test)

lispMultipleTest1 :: Test
lispMultipleTest1 = lispProcedureTest lispMultiple
    [LispInteger 1, LispInteger 2, LispInteger 3]
    (return (LispInteger 6))

lispMultipleTest2 :: Test
lispMultipleTest2 = lispProcedureTest lispMultiple
    [LispInteger 1, LispInteger 2]
    (return (LispInteger 2))

lispMultipleTest3 :: Test
lispMultipleTest3 = lispProcedureTest lispMultiple
    [LispInteger 1]
    (return (LispInteger 1))

lispMultipleTest4 :: Test
lispMultipleTest4 = lispProcedureTest lispMultiple
    []
    (return (LispInteger 1))

lispMultipleTest5 :: Test
lispMultipleTest5 = lispProcedureTest lispMultiple
    [LispInteger 1234567890123456789, LispInteger 9876543210987654321]
    (return (LispInteger 12193263113702179522374638011112635269))

lispMultipleTest6 :: Test
lispMultipleTest6 = lispProcedureTest lispMultiple
    [LispReal 1.3, LispInteger (-5)]
    (return (LispReal (-6.5)))

lispMultipleTest7 :: Test
lispMultipleTest7 = lispProcedureTest lispMultiple
    [LispRational (3 % 4), LispRational (7 % 9)]
    (return (LispRational (7 % 12)))

lispAdditionTest1 :: Test
lispAdditionTest1 = lispProcedureTest lispAddition
    [LispInteger 1, LispInteger 2, LispInteger 3]
    (return (LispInteger 6))

lispAdditionTest2 :: Test
lispAdditionTest2 = lispProcedureTest lispAddition
    [LispInteger 1, LispInteger 2]
    (return (LispInteger 3))

lispAdditionTest3 :: Test
lispAdditionTest3 = lispProcedureTest lispAddition
    [LispInteger 1]
    (return (LispInteger 1))

lispAdditionTest4 :: Test
lispAdditionTest4 = lispProcedureTest lispAddition
    []
    (return (LispInteger 0))

lispAdditionTest5 :: Test
lispAdditionTest5 = lispProcedureTest lispAddition
    [LispInteger 1234567890123456789, LispInteger 9876543210987654321]
    (return (LispInteger 11111111101111111110))

lispAdditionTest6 :: Test
lispAdditionTest6 = lispProcedureTest lispAddition
    [LispReal 1.3, LispInteger (-5)]
    (return (LispReal (-3.7)))

lispAdditionTest7 :: Test
lispAdditionTest7 = lispProcedureTest lispAddition
    [LispRational (3 % 4), LispRational (7 % 9)]
    (return (LispRational (55 % 36)))

lispSubtractTest1 :: Test
lispSubtractTest1 = lispProcedureTest lispSubtract
    [LispInteger 1, LispInteger 2, LispInteger 3]
    (return (LispInteger (-4)))

lispSubtractTest2 :: Test
lispSubtractTest2 = lispProcedureTest lispSubtract
    [LispInteger 1, LispInteger 2]
    (return (LispInteger (-1)))

lispSubtractTest3 :: Test
lispSubtractTest3 = lispProcedureTest lispSubtract
    [LispInteger 1]
    (return (LispInteger (-1)))

lispSubtractTest4 :: Test
lispSubtractTest4 = lispProcedureTest lispSubtract
    [LispInteger 1234567890123456789, LispInteger 9876543210987654321]
    (return (LispInteger (-8641975320864197532)))

lispSubtractTest5 :: Test
lispSubtractTest5 = lispProcedureTest lispSubtract
    [LispReal 1.3, LispInteger (-5)]
    (return (LispReal 6.3))

lispSubtractTest6 :: Test
lispSubtractTest6 = lispProcedureTest lispSubtract
    [LispRational (3 % 4), LispRational (7 % 9)]
    (return (LispRational (- (1 % 36))))

lispDivisionTest1 :: Test
lispDivisionTest1 = lispProcedureTest lispDivision
    [LispInteger 10]
    (return (LispRational (1 % 10)))

lispDivisionTest2 :: Test
lispDivisionTest2 = lispProcedureTest lispDivision
    [LispReal 10.0]
    (return (LispReal 0.1))

lispDivisionTest3 :: Test
lispDivisionTest3 = lispProcedureTest lispDivision
    [LispInteger 10, LispInteger 2]
    (return (LispInteger 5))

lispDivisionTest4 :: Test
lispDivisionTest4 = lispProcedureTest lispDivision
    [LispInteger 2, LispInteger 10]
    (return (LispRational (1 % 5)))

lispDivisionTest5 :: Test
lispDivisionTest5 = lispProcedureTest lispDivision
    [LispInteger 100, LispInteger 2, LispInteger 5, LispInteger 2]
    (return (LispInteger 5))

lispDivisionTest6 :: Test
lispDivisionTest6 = lispProcedureTest lispDivision
    [LispInteger 1234567890123456789, LispInteger 9876543210987654321]
    (return (LispRational (13717421 % 109739369)))

lispDivisionTest7 :: Test
lispDivisionTest7 = lispProcedureTest lispDivision
    [LispReal 1.3, LispInteger (-5)]
    (return (LispReal (-0.26)))

lispDivisionTest8 :: Test
lispDivisionTest8 = lispProcedureTest lispDivision
    [LispRational (3 % 4), LispRational (7 % 9)]
    (return (LispRational (27 % 28)))

lispLessThanTest1 :: Test
lispLessThanTest1 = lispProcedureTest lispLessThan
    [LispInteger 1, LispInteger 2]
    (return (LispBool True))

lispLessThanTest2 :: Test
lispLessThanTest2 = lispProcedureTest lispLessThan
    [LispInteger 2, LispInteger 1]
    (return (LispBool False))

lispLessThanTest3 :: Test
lispLessThanTest3 = lispProcedureTest lispLessThan
    [LispInteger 2, LispReal 2.001]
    (return (LispBool True))

lispLessThanTest4 :: Test
lispLessThanTest4 = lispProcedureTest lispLessThan
    [LispInteger 2, LispInteger 2]
    (return (LispBool False))

lispLessThanTest5 :: Test
lispLessThanTest5 = lispProcedureTest lispLessThan
    [LispInteger 1234567890123456789, LispInteger 9876543210987654321]
    (return (LispBool True))

lispLessThanTest6 :: Test
lispLessThanTest6 = lispProcedureTest lispLessThan
    [LispInteger 1, LispInteger 2, LispInteger 3, LispInteger 4, LispInteger 5]
    (return (LispBool True))

lispLessThanTest7 :: Test
lispLessThanTest7 = lispProcedureTest lispLessThan
    [LispInteger 1, LispInteger 2, LispInteger 4, LispInteger 3, LispInteger 5]
    (return (LispBool False))

lispLessThanTest8 :: Test
lispLessThanTest8 = lispProcedureTest lispLessThan
    [LispInteger 1, LispInteger 2, LispInteger 4, LispInteger 4, LispInteger 5]
    (return (LispBool False))

lispLessThanTest9 :: Test
lispLessThanTest9 = lispProcedureTest lispLessThan
    [LispRational (3 % 4), LispRational (7 % 9)]
    (return (LispBool True))

lispLessThanTest10 :: Test
lispLessThanTest10 = lispProcedureTest lispLessThan
    [LispInteger 5]
    (return (LispBool True))

lispLessThanOrEqTest1 :: Test
lispLessThanOrEqTest1 = lispProcedureTest lispLessThanOrEq
    [LispInteger 1, LispInteger 2]
    (return (LispBool True))

lispLessThanOrEqTest2 :: Test
lispLessThanOrEqTest2 = lispProcedureTest lispLessThanOrEq
    [LispInteger 2, LispInteger 1]
    (return (LispBool False))

lispLessThanOrEqTest3 :: Test
lispLessThanOrEqTest3 = lispProcedureTest lispLessThanOrEq
    [LispInteger 2, LispReal 2.001]
    (return (LispBool True))

lispLessThanOrEqTest4 :: Test
lispLessThanOrEqTest4 = lispProcedureTest lispLessThanOrEq
    [LispInteger 2, LispInteger 2]
    (return (LispBool True))

lispLessThanOrEqTest5 :: Test
lispLessThanOrEqTest5 = lispProcedureTest lispLessThanOrEq
    [LispInteger 1234567890123456789, LispInteger 9876543210987654321]
    (return (LispBool True))

lispLessThanOrEqTest6 :: Test
lispLessThanOrEqTest6 = lispProcedureTest lispLessThanOrEq
    [LispInteger 1, LispInteger 2, LispInteger 3, LispInteger 4, LispInteger 5]
    (return (LispBool True))

lispLessThanOrEqTest7 :: Test
lispLessThanOrEqTest7 = lispProcedureTest lispLessThanOrEq
    [LispInteger 1, LispInteger 2, LispInteger 4, LispInteger 3, LispInteger 5]
    (return (LispBool False))

lispLessThanOrEqTest8 :: Test
lispLessThanOrEqTest8 = lispProcedureTest lispLessThanOrEq
    [LispInteger 1, LispInteger 2, LispInteger 4, LispInteger 4, LispInteger 5]
    (return (LispBool True))

lispLessThanOrEqTest9 :: Test
lispLessThanOrEqTest9 = lispProcedureTest lispLessThanOrEq
    [LispRational (3 % 4), LispRational (7 % 9)]
    (return (LispBool True))

lispLessThanOrEqTest10 :: Test
lispLessThanOrEqTest10 = lispProcedureTest lispLessThanOrEq
    [LispInteger 5]
    (return (LispBool True))

lispNumberEqTest1 :: Test
lispNumberEqTest1 = lispProcedureTest lispNumberEq
    [LispInteger 1, LispInteger 2]
    (return (LispBool False))

lispNumberEqTest2 :: Test
lispNumberEqTest2 = lispProcedureTest lispNumberEq
    [LispInteger 2, LispInteger 1]
    (return (LispBool False))

lispNumberEqTest3 :: Test
lispNumberEqTest3 = lispProcedureTest lispNumberEq
    [LispInteger 2, LispReal 2.001]
    (return (LispBool False))

lispNumberEqTest4 :: Test
lispNumberEqTest4 = lispProcedureTest lispNumberEq
    [LispInteger 2, LispInteger 2]
    (return (LispBool True))

lispNumberEqTest5 :: Test
lispNumberEqTest5 = lispProcedureTest lispNumberEq
    [LispInteger 2, LispReal 2.0]
    (return (LispBool True))

lispNumberEqTest6 :: Test
lispNumberEqTest6 = lispProcedureTest lispNumberEq
    [LispReal 0.0, LispReal (-0.0)]
    (return (LispBool True))

lispNumberEqTest7 :: Test
lispNumberEqTest7 = lispProcedureTest lispNumberEq
    [LispInteger 1, LispInteger 2, LispInteger 3, LispInteger 4, LispInteger 5]
    (return (LispBool False))

lispNumberEqTest8 :: Test
lispNumberEqTest8 = lispProcedureTest lispNumberEq
    [LispInteger 4, LispInteger 4, LispInteger 4, LispInteger 3, LispInteger 4]
    (return (LispBool False))

lispNumberEqTest9 :: Test
lispNumberEqTest9 = lispProcedureTest lispNumberEq
    [LispInteger 4, LispInteger 4, LispInteger 4, LispInteger 4, LispInteger 4]
    (return (LispBool True))

lispNumberEqTest10 :: Test
lispNumberEqTest10 = lispProcedureTest lispNumberEq
    [LispInteger 4, LispInteger 4, LispInteger 4, LispReal 4.0, LispInteger 4]
    (return (LispBool True))

lispNumberEqTest11 :: Test
lispNumberEqTest11 = lispProcedureTest lispNumberEq
    [LispInteger 5]
    (return (LispBool True))

lispGreaterThanTest1 :: Test
lispGreaterThanTest1 = lispProcedureTest lispGreaterThan
    [LispInteger 2, LispInteger 1]
    (return (LispBool True))

lispGreaterThanTest2 :: Test
lispGreaterThanTest2 = lispProcedureTest lispGreaterThan
    [LispInteger 1, LispInteger 2]
    (return (LispBool False))

lispGreaterThanTest3 :: Test
lispGreaterThanTest3 = lispProcedureTest lispGreaterThan
    [LispReal 2.001, LispInteger 2]
    (return (LispBool True))

lispGreaterThanTest4 :: Test
lispGreaterThanTest4 = lispProcedureTest lispGreaterThan
    [LispInteger 2, LispInteger 2]
    (return (LispBool False))

lispGreaterThanTest5 :: Test
lispGreaterThanTest5 = lispProcedureTest lispGreaterThan
    [LispInteger 9876543210987654321, LispInteger 1234567890123456789]
    (return (LispBool True))

lispGreaterThanTest6 :: Test
lispGreaterThanTest6 = lispProcedureTest lispGreaterThan
    [LispInteger 5, LispInteger 4, LispInteger 3, LispInteger 2, LispInteger 1]
    (return (LispBool True))

lispGreaterThanTest7 :: Test
lispGreaterThanTest7 = lispProcedureTest lispGreaterThan
    [LispInteger 5, LispInteger 3, LispInteger 4, LispInteger 2, LispInteger 1]
    (return (LispBool False))

lispGreaterThanTest8 :: Test
lispGreaterThanTest8 = lispProcedureTest lispGreaterThan
    [LispInteger 5, LispInteger 4, LispInteger 4, LispInteger 2, LispInteger 1]
    (return (LispBool False))

lispGreaterThanTest9 :: Test
lispGreaterThanTest9 = lispProcedureTest lispGreaterThan
    [LispRational (7 % 9), LispRational (3 % 4)]
    (return (LispBool True))

lispGreaterThanTest10 :: Test
lispGreaterThanTest10 = lispProcedureTest lispGreaterThan
    [LispInteger 5]
    (return (LispBool True))

lispGreaterThanOrEqTest1 :: Test
lispGreaterThanOrEqTest1 = lispProcedureTest lispGreaterThanOrEq
    [LispInteger 2, LispInteger 1]
    (return (LispBool True))

lispGreaterThanOrEqTest2 :: Test
lispGreaterThanOrEqTest2 = lispProcedureTest lispGreaterThanOrEq
    [LispInteger 1, LispInteger 2]
    (return (LispBool False))

lispGreaterThanOrEqTest3 :: Test
lispGreaterThanOrEqTest3 = lispProcedureTest lispGreaterThanOrEq
    [LispReal 2.001, LispInteger 2]
    (return (LispBool True))

lispGreaterThanOrEqTest4 :: Test
lispGreaterThanOrEqTest4 = lispProcedureTest lispGreaterThanOrEq
    [LispInteger 2, LispInteger 2]
    (return (LispBool True))

lispGreaterThanOrEqTest5 :: Test
lispGreaterThanOrEqTest5 = lispProcedureTest lispGreaterThanOrEq
    [LispInteger 9876543210987654321, LispInteger 1234567890123456789]
    (return (LispBool True))

lispGreaterThanOrEqTest6 :: Test
lispGreaterThanOrEqTest6 = lispProcedureTest lispGreaterThanOrEq
    [LispInteger 5, LispInteger 4, LispInteger 3, LispInteger 2, LispInteger 1]
    (return (LispBool True))

lispGreaterThanOrEqTest7 :: Test
lispGreaterThanOrEqTest7 = lispProcedureTest lispGreaterThanOrEq
    [LispInteger 5, LispInteger 3, LispInteger 4, LispInteger 2, LispInteger 1]
    (return (LispBool False))

lispGreaterThanOrEqTest8 :: Test
lispGreaterThanOrEqTest8 = lispProcedureTest lispGreaterThanOrEq
    [LispInteger 5, LispInteger 4, LispInteger 4, LispInteger 2, LispInteger 1]
    (return (LispBool True))

lispGreaterThanOrEqTest9 :: Test
lispGreaterThanOrEqTest9 = lispProcedureTest lispGreaterThanOrEq
    [LispRational (7 % 9), LispRational (3 % 4)]
    (return (LispBool True))

lispGreaterThanOrEqTest10 :: Test
lispGreaterThanOrEqTest10 = lispProcedureTest lispGreaterThanOrEq
    [LispInteger 5]
    (return (LispBool True))

lispABSTest1 :: Test
lispABSTest1 = lispProcedureTest lispABS
    [LispInteger 0]
    (return (LispInteger 0))

lispABSTest2 :: Test
lispABSTest2 = lispProcedureTest lispABS
    [LispRational (12 % 13)]
    (return (LispRational (12 % 13)))

lispABSTest3 :: Test
lispABSTest3 = lispProcedureTest lispABS
    [LispReal (-1.09)]
    (return (LispReal 1.09))

lispCOSTest1 :: Test
lispCOSTest1 = lispProcedureTest lispCOS
    [LispReal 0.0]
    (return (LispReal 1.0))

lispCOSTest2 :: Test
lispCOSTest2 = lispProcedureTest lispCOS
    [LispReal 1.0]
    (return (LispReal 0.5403023))

lispEXPTTest1 :: Test
lispEXPTTest1 = lispProcedureTest lispEXPT
    [LispInteger 2, LispInteger 8]
    (return (LispInteger 256))

lispEXPTTest2 :: Test
lispEXPTTest2 = lispProcedureTest lispEXPT
    [LispInteger 2, LispInteger 32]
    (return (LispInteger 4294967296))

lispEXPTTest3 :: Test
lispEXPTTest3 = lispProcedureTest lispEXPT
    [LispInteger 2, LispInteger 64]
    (return (LispInteger 18446744073709551616))

lispEXPTTest4 :: Test
lispEXPTTest4 = lispProcedureTest lispEXPT
    [LispInteger 10, LispInteger 3]
    (return (LispInteger 1000))

lispEXPTTest5 :: Test
lispEXPTTest5 = lispProcedureTest lispEXPT
    [LispInteger 5, LispRational (1 % 3)]
    (return (LispReal 1.709976))

lispEXPTTest6 :: Test
lispEXPTTest6 = lispProcedureTest lispEXPT
    [LispReal 1.709976, LispInteger 3]
    (return (LispReal 5.0))

lispEXPTTest7 :: Test
lispEXPTTest7 = lispProcedureTest lispEXPT
    [LispInteger 1, LispInteger 2]
    (return (LispInteger 1))

lispEXPTTest8 :: Test
lispEXPTTest8 = lispProcedureTest lispEXPT
    [LispReal 1.0, LispInteger 2]
    (return (LispReal 1.0))

lispEXPTTest9 :: Test
lispEXPTTest9 = lispProcedureTest lispEXPT
    [LispInteger (-2), LispInteger 5]
    (return (LispInteger (-32)))

lispEXPTTest10 :: Test
lispEXPTTest10 = lispProcedureTest lispEXPT
    [LispInteger 2, LispReal 2.5]
    (return (LispReal 5.656854))

lispEXPTTest11 :: Test
lispEXPTTest11 = lispProcedureTest lispEXPT
    [LispInteger 2, LispInteger (-3)]
    (return (LispRational (1 % 8)))

lispEXPTTest12 :: Test
lispEXPTTest12 = lispProcedureTest lispEXPT
    [LispInteger (-2), LispInteger (-3)]
    (return (LispRational ((-1) % 8)))

lispEXPTTest13 :: Test
lispEXPTTest13 = lispProcedureTest lispEXPT
    [LispReal (-2.4), LispInteger (-3)]
    (return (LispReal (-0.072337955)))

lispMAXTest1 :: Test
lispMAXTest1 = lispProcedureTest lispMAX
    [LispInteger 1, LispInteger 3, LispInteger 2]
    (return (LispInteger 3))

lispMAXTest2 :: Test
lispMAXTest2 = lispProcedureTest lispMAX
    [LispInteger 4]
    (return (LispInteger 4))

lispMINTest1 :: Test
lispMINTest1 = lispProcedureTest lispMIN
    [LispInteger 1, LispInteger 3, LispInteger 2]
    (return (LispInteger 1))

lispMINTest2 :: Test
lispMINTest2 = lispProcedureTest lispMIN
    [LispInteger 4]
    (return (LispInteger 4))

lispNOTTest1 :: Test
lispNOTTest1 = lispProcedureTest lispNOT
    [LispBool True]
    (return (LispBool False))

lispNOTTest2 :: Test
lispNOTTest2 = lispProcedureTest lispNOT
    [LispBool False]
    (return (LispBool True))

lispNOTTest3 :: Test
lispNOTTest3 = lispProcedureTest lispNOT
    [LispReal 234.3]
    (return (LispBool False))

lispSINTest1 :: Test
lispSINTest1 = lispProcedureTest lispSIN
    [LispReal 0.0]
    (return (LispReal 0.0))

lispSINTest2 :: Test
lispSINTest2 = lispProcedureTest lispSIN
    [LispReal 1.0]
    (return (LispReal 0.84147096))

lispSQRTTest1 :: Test
lispSQRTTest1 = lispProcedureTest lispSQRT
    [LispInteger 10]
    (return (LispReal 3.1622777))

lispSQRTTest2 :: Test
lispSQRTTest2 = lispProcedureTest lispSQRT
    [LispReal 10.0]
    (return (LispReal 3.1622777))

lispSQRTTest3 :: Test
lispSQRTTest3 = lispProcedureTest lispSQRT
    [LispInteger 4]
    (return (LispReal 2.0))

lispNUMBERPTest1 :: Test
lispNUMBERPTest1 = lispProcedureTest lispNUMBERP
    [LispInteger 12]
    (return (LispBool True))

lispNUMBERPTest2 :: Test
lispNUMBERPTest2 = lispProcedureTest lispNUMBERP
    [LispBool False]
    (return (LispBool False))
