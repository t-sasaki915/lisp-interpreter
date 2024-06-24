{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module LispMathsSpec where

import LispData (LispData(..))
import LispEnv (Evalable, Eval)
import LispInterpreter (initEnv)
import LispMaths
import SpecUtil

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Strict (runStateT)
import Data.Ratio ((%))
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit (Test(TestCase), assertEqual)

lispMathsTest :: Evalable -> [LispData] -> Eval -> Test
lispMathsTest f args e =
    let
        result =
            fst $ unsafePerformIO $ runStateT (runExceptT (f 0 args)) initEnv
        e' =
            fst $ unsafePerformIO $ runStateT (runExceptT e) initEnv
    in
    TestCase $ assertEqual "" result e'

lispMultipleTest1 :: Test
lispMultipleTest1 = lispMathsTest lispMultiple
    [integer 1, integer 2, integer 3]
    (return (integer 6))

lispMultipleTest2 :: Test
lispMultipleTest2 = lispMathsTest lispMultiple
    [integer 1, integer 2]
    (return (integer 2))

lispMultipleTest3 :: Test
lispMultipleTest3 = lispMathsTest lispMultiple
    [integer 1]
    (return (integer 1))

lispMultipleTest4 :: Test
lispMultipleTest4 = lispMathsTest lispMultiple
    []
    (return (integer 1))

lispMultipleTest5 :: Test
lispMultipleTest5 = lispMathsTest lispMultiple
    [integer 1234567890123456789, integer 9876543210987654321]
    (return (integer 12193263113702179522374638011112635269))

lispMultipleTest6 :: Test
lispMultipleTest6 = lispMathsTest lispMultiple
    [real 1.3, integer (-5)]
    (return (real (-6.5)))

lispMultipleTest7 :: Test
lispMultipleTest7 = lispMathsTest lispMultiple
    [rational (3 % 4), rational (7 % 9)]
    (return (rational (7 % 12)))

lispAdditionTest1 :: Test
lispAdditionTest1 = lispMathsTest lispAddition
    [integer 1, integer 2, integer 3]
    (return (integer 6))

lispAdditionTest2 :: Test
lispAdditionTest2 = lispMathsTest lispAddition
    [integer 1, integer 2]
    (return (integer 3))

lispAdditionTest3 :: Test
lispAdditionTest3 = lispMathsTest lispAddition
    [integer 1]
    (return (integer 1))

lispAdditionTest4 :: Test
lispAdditionTest4 = lispMathsTest lispAddition
    []
    (return (integer 0))

lispAdditionTest5 :: Test
lispAdditionTest5 = lispMathsTest lispAddition
    [integer 1234567890123456789, integer 9876543210987654321]
    (return (integer 11111111101111111110))

lispAdditionTest6 :: Test
lispAdditionTest6 = lispMathsTest lispAddition
    [real 1.3, integer (-5)]
    (return (real (-3.7)))

lispAdditionTest7 :: Test
lispAdditionTest7 = lispMathsTest lispAddition
    [rational (3 % 4), rational (7 % 9)]
    (return (rational (55 % 36)))

lispSubtractTest1 :: Test
lispSubtractTest1 = lispMathsTest lispSubtract
    [integer 1, integer 2, integer 3]
    (return (integer (-4)))

lispSubtractTest2 :: Test
lispSubtractTest2 = lispMathsTest lispSubtract
    [integer 1, integer 2]
    (return (integer (-1)))

lispSubtractTest3 :: Test
lispSubtractTest3 = lispMathsTest lispSubtract
    [integer 1]
    (return (integer (-1)))

lispSubtractTest4 :: Test
lispSubtractTest4 = lispMathsTest lispSubtract
    [integer 1234567890123456789, integer 9876543210987654321]
    (return (integer (-8641975320864197532)))

lispSubtractTest5 :: Test
lispSubtractTest5 = lispMathsTest lispSubtract
    [real 1.3, integer (-5)]
    (return (real 6.3))

lispSubtractTest6 :: Test
lispSubtractTest6 = lispMathsTest lispSubtract
    [rational (3 % 4), rational (7 % 9)]
    (return (rational (- (1 % 36))))

lispDivisionTest1 :: Test
lispDivisionTest1 = lispMathsTest lispDivision
    [integer 10]
    (return (rational (1 % 10)))

lispDivisionTest2 :: Test
lispDivisionTest2 = lispMathsTest lispDivision
    [real 10.0]
    (return (real 0.1))

lispDivisionTest3 :: Test
lispDivisionTest3 = lispMathsTest lispDivision
    [integer 10, integer 2]
    (return (integer 5))

lispDivisionTest4 :: Test
lispDivisionTest4 = lispMathsTest lispDivision
    [integer 2, integer 10]
    (return (rational (1 % 5)))

lispDivisionTest5 :: Test
lispDivisionTest5 = lispMathsTest lispDivision
    [integer 100, integer 2, integer 5, integer 2]
    (return (integer 5))

lispDivisionTest6 :: Test
lispDivisionTest6 = lispMathsTest lispDivision
    [integer 1234567890123456789, integer 9876543210987654321]
    (return (rational (13717421 % 109739369)))

lispDivisionTest7 :: Test
lispDivisionTest7 = lispMathsTest lispDivision
    [real 1.3, integer (-5)]
    (return (real (-0.26)))

lispDivisionTest8 :: Test
lispDivisionTest8 = lispMathsTest lispDivision
    [rational (3 % 4), rational (7 % 9)]
    (return (rational (27 % 28)))

lispLessThanTest1 :: Test
lispLessThanTest1 = lispMathsTest lispLessThan
    [integer 1, integer 2]
    (return (boolean True))

lispLessThanTest2 :: Test
lispLessThanTest2 = lispMathsTest lispLessThan
    [integer 2, integer 1]
    (return (boolean False))

lispLessThanTest3 :: Test
lispLessThanTest3 = lispMathsTest lispLessThan
    [integer 2, real 2.001]
    (return (boolean True))

lispLessThanTest4 :: Test
lispLessThanTest4 = lispMathsTest lispLessThan
    [integer 2, integer 2]
    (return (boolean False))

lispLessThanTest5 :: Test
lispLessThanTest5 = lispMathsTest lispLessThan
    [integer 1234567890123456789, integer 9876543210987654321]
    (return (boolean True))

lispLessThanTest6 :: Test
lispLessThanTest6 = lispMathsTest lispLessThan
    [integer 1, integer 2, integer 3, integer 4, integer 5]
    (return (boolean True))

lispLessThanTest7 :: Test
lispLessThanTest7 = lispMathsTest lispLessThan
    [integer 1, integer 2, integer 4, integer 3, integer 5]
    (return (boolean False))

lispLessThanTest8 :: Test
lispLessThanTest8 = lispMathsTest lispLessThan
    [integer 1, integer 2, integer 4, integer 4, integer 5]
    (return (boolean False))

lispLessThanTest9 :: Test
lispLessThanTest9 = lispMathsTest lispLessThan
    [rational (3 % 4), rational (7 % 9)]
    (return (boolean True))

lispLessThanTest10 :: Test
lispLessThanTest10 = lispMathsTest lispLessThan
    [integer 5]
    (return (boolean True))

lispLessThanOrEqTest1 :: Test
lispLessThanOrEqTest1 = lispMathsTest lispLessThanOrEq
    [integer 1, integer 2]
    (return (boolean True))

lispLessThanOrEqTest2 :: Test
lispLessThanOrEqTest2 = lispMathsTest lispLessThanOrEq
    [integer 2, integer 1]
    (return (boolean False))

lispLessThanOrEqTest3 :: Test
lispLessThanOrEqTest3 = lispMathsTest lispLessThanOrEq
    [integer 2, real 2.001]
    (return (boolean True))

lispLessThanOrEqTest4 :: Test
lispLessThanOrEqTest4 = lispMathsTest lispLessThanOrEq
    [integer 2, integer 2]
    (return (boolean True))

lispLessThanOrEqTest5 :: Test
lispLessThanOrEqTest5 = lispMathsTest lispLessThanOrEq
    [integer 1234567890123456789, integer 9876543210987654321]
    (return (boolean True))

lispLessThanOrEqTest6 :: Test
lispLessThanOrEqTest6 = lispMathsTest lispLessThanOrEq
    [integer 1, integer 2, integer 3, integer 4, integer 5]
    (return (boolean True))

lispLessThanOrEqTest7 :: Test
lispLessThanOrEqTest7 = lispMathsTest lispLessThanOrEq
    [integer 1, integer 2, integer 4, integer 3, integer 5]
    (return (boolean False))

lispLessThanOrEqTest8 :: Test
lispLessThanOrEqTest8 = lispMathsTest lispLessThanOrEq
    [integer 1, integer 2, integer 4, integer 4, integer 5]
    (return (boolean True))

lispLessThanOrEqTest9 :: Test
lispLessThanOrEqTest9 = lispMathsTest lispLessThanOrEq
    [rational (3 % 4), rational (7 % 9)]
    (return (boolean True))

lispLessThanOrEqTest10 :: Test
lispLessThanOrEqTest10 = lispMathsTest lispLessThanOrEq
    [integer 5]
    (return (boolean True))

lispNumberEqTest1 :: Test
lispNumberEqTest1 = lispMathsTest lispNumberEq
    [integer 1, integer 2]
    (return (boolean False))

lispNumberEqTest2 :: Test
lispNumberEqTest2 = lispMathsTest lispNumberEq
    [integer 2, integer 1]
    (return (boolean False))

lispNumberEqTest3 :: Test
lispNumberEqTest3 = lispMathsTest lispNumberEq
    [integer 2, real 2.001]
    (return (boolean False))

lispNumberEqTest4 :: Test
lispNumberEqTest4 = lispMathsTest lispNumberEq
    [integer 2, integer 2]
    (return (boolean True))

lispNumberEqTest5 :: Test
lispNumberEqTest5 = lispMathsTest lispNumberEq
    [integer 2, real 2.0]
    (return (boolean True))

lispNumberEqTest6 :: Test
lispNumberEqTest6 = lispMathsTest lispNumberEq
    [real 0.0, real (-0.0)]
    (return (boolean True))

lispNumberEqTest7 :: Test
lispNumberEqTest7 = lispMathsTest lispNumberEq
    [integer 1, integer 2, integer 3, integer 4, integer 5]
    (return (boolean False))

lispNumberEqTest8 :: Test
lispNumberEqTest8 = lispMathsTest lispNumberEq
    [integer 4, integer 4, integer 4, integer 3, integer 4]
    (return (boolean False))

lispNumberEqTest9 :: Test
lispNumberEqTest9 = lispMathsTest lispNumberEq
    [integer 4, integer 4, integer 4, integer 4, integer 4]
    (return (boolean True))

lispNumberEqTest10 :: Test
lispNumberEqTest10 = lispMathsTest lispNumberEq
    [integer 4, integer 4, integer 4, real 4.0, integer 4]
    (return (boolean True))

lispNumberEqTest11 :: Test
lispNumberEqTest11 = lispMathsTest lispNumberEq
    [integer 5]
    (return (boolean True))

lispGreaterThanTest1 :: Test
lispGreaterThanTest1 = lispMathsTest lispGreaterThan
    [integer 2, integer 1]
    (return (boolean True))

lispGreaterThanTest2 :: Test
lispGreaterThanTest2 = lispMathsTest lispGreaterThan
    [integer 1, integer 2]
    (return (boolean False))

lispGreaterThanTest3 :: Test
lispGreaterThanTest3 = lispMathsTest lispGreaterThan
    [real 2.001, integer 2]
    (return (boolean True))

lispGreaterThanTest4 :: Test
lispGreaterThanTest4 = lispMathsTest lispGreaterThan
    [integer 2, integer 2]
    (return (boolean False))

lispGreaterThanTest5 :: Test
lispGreaterThanTest5 = lispMathsTest lispGreaterThan
    [integer 9876543210987654321, integer 1234567890123456789]
    (return (boolean True))

lispGreaterThanTest6 :: Test
lispGreaterThanTest6 = lispMathsTest lispGreaterThan
    [integer 5, integer 4, integer 3, integer 2, integer 1]
    (return (boolean True))

lispGreaterThanTest7 :: Test
lispGreaterThanTest7 = lispMathsTest lispGreaterThan
    [integer 5, integer 3, integer 4, integer 2, integer 1]
    (return (boolean False))

lispGreaterThanTest8 :: Test
lispGreaterThanTest8 = lispMathsTest lispGreaterThan
    [integer 5, integer 4, integer 4, integer 2, integer 1]
    (return (boolean False))

lispGreaterThanTest9 :: Test
lispGreaterThanTest9 = lispMathsTest lispGreaterThan
    [rational (7 % 9), rational (3 % 4)]
    (return (boolean True))

lispGreaterThanTest10 :: Test
lispGreaterThanTest10 = lispMathsTest lispGreaterThan
    [integer 5]
    (return (boolean True))

lispGreaterThanOrEqTest1 :: Test
lispGreaterThanOrEqTest1 = lispMathsTest lispGreaterThanOrEq
    [integer 2, integer 1]
    (return (boolean True))

lispGreaterThanOrEqTest2 :: Test
lispGreaterThanOrEqTest2 = lispMathsTest lispGreaterThanOrEq
    [integer 1, integer 2]
    (return (boolean False))

lispGreaterThanOrEqTest3 :: Test
lispGreaterThanOrEqTest3 = lispMathsTest lispGreaterThanOrEq
    [real 2.001, integer 2]
    (return (boolean True))

lispGreaterThanOrEqTest4 :: Test
lispGreaterThanOrEqTest4 = lispMathsTest lispGreaterThanOrEq
    [integer 2, integer 2]
    (return (boolean True))

lispGreaterThanOrEqTest5 :: Test
lispGreaterThanOrEqTest5 = lispMathsTest lispGreaterThanOrEq
    [integer 9876543210987654321, integer 1234567890123456789]
    (return (boolean True))

lispGreaterThanOrEqTest6 :: Test
lispGreaterThanOrEqTest6 = lispMathsTest lispGreaterThanOrEq
    [integer 5, integer 4, integer 3, integer 2, integer 1]
    (return (boolean True))

lispGreaterThanOrEqTest7 :: Test
lispGreaterThanOrEqTest7 = lispMathsTest lispGreaterThanOrEq
    [integer 5, integer 3, integer 4, integer 2, integer 1]
    (return (boolean False))

lispGreaterThanOrEqTest8 :: Test
lispGreaterThanOrEqTest8 = lispMathsTest lispGreaterThanOrEq
    [integer 5, integer 4, integer 4, integer 2, integer 1]
    (return (boolean True))

lispGreaterThanOrEqTest9 :: Test
lispGreaterThanOrEqTest9 = lispMathsTest lispGreaterThanOrEq
    [rational (7 % 9), rational (3 % 4)]
    (return (boolean True))

lispGreaterThanOrEqTest10 :: Test
lispGreaterThanOrEqTest10 = lispMathsTest lispGreaterThanOrEq
    [integer 5]
    (return (boolean True))

lispABSTest1 :: Test
lispABSTest1 = lispMathsTest lispABS
    [integer 0]
    (return (integer 0))

lispABSTest2 :: Test
lispABSTest2 = lispMathsTest lispABS
    [rational (12 % 13)]
    (return (rational (12 % 13)))

lispABSTest3 :: Test
lispABSTest3 = lispMathsTest lispABS
    [real (-1.09)]
    (return (real 1.09))

lispCOSTest1 :: Test
lispCOSTest1 = lispMathsTest lispCOS
    [real 0.0]
    (return (real 1.0))

lispCOSTest2 :: Test
lispCOSTest2 = lispMathsTest lispCOS
    [real 1.0]
    (return (real 0.5403023))

lispNOTTest1 :: Test
lispNOTTest1 = lispMathsTest lispNOT
    [boolean True]
    (return (boolean False))

lispNOTTest2 :: Test
lispNOTTest2 = lispMathsTest lispNOT
    [boolean False]
    (return (boolean True))

lispNOTTest3 :: Test
lispNOTTest3 = lispMathsTest lispNOT
    [real 234.3]
    (return (boolean False))

lispSINTest1 :: Test
lispSINTest1 = lispMathsTest lispSIN
    [real 0.0]
    (return (real 0.0))

lispSINTest2 :: Test
lispSINTest2 = lispMathsTest lispSIN
    [real 1.0]
    (return (real 0.84147096))

lispSQRTTest1 :: Test
lispSQRTTest1 = lispMathsTest lispSQRT
    [integer 10]
    (return (real 3.1622777))

lispSQRTTest2 :: Test
lispSQRTTest2 = lispMathsTest lispSQRT
    [real 10.0]
    (return (real 3.1622777))

lispSQRTTest3 :: Test
lispSQRTTest3 = lispMathsTest lispSQRT
    [integer 4]
    (return (real 2.0))

lispNUMBERPTest1 :: Test
lispNUMBERPTest1 = lispMathsTest lispNUMBERP
    [integer 12]
    (return (boolean True))

lispNUMBERPTest2 :: Test
lispNUMBERPTest2 = lispMathsTest lispNUMBERP
    [boolean False]
    (return (boolean False))
