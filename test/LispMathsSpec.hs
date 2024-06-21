{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module LispMathsSpec where

import LispData (LispData(..))
import LispEnv (Evalable, Eval)
import LispInterpreter (initEnv)
import LispMaths

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
    [LispInteger 0 1, LispInteger 0 2, LispInteger 0 3]
    (return (LispInteger 0 6))

lispMultipleTest2 :: Test
lispMultipleTest2 = lispMathsTest lispMultiple
    [LispInteger 0 1, LispInteger 0 2]
    (return (LispInteger 0 2))

lispMultipleTest3 :: Test
lispMultipleTest3 = lispMathsTest lispMultiple
    [LispInteger 0 1]
    (return (LispInteger 0 1))

lispMultipleTest4 :: Test
lispMultipleTest4 = lispMathsTest lispMultiple
    []
    (return (LispInteger 0 1))

lispMultipleTest5 :: Test
lispMultipleTest5 = lispMathsTest lispMultiple
    [LispInteger 0 1234567890123456789, LispInteger 0 9876543210987654321]
    (return (LispInteger 0 12193263113702179522374638011112635269))

lispMultipleTest6 :: Test
lispMultipleTest6 = lispMathsTest lispMultiple
    [LispReal 0 1.3, LispInteger 0 (-5)]
    (return (LispReal 0 (-6.5)))

lispMultipleTest7 :: Test
lispMultipleTest7 = lispMathsTest lispMultiple
    [LispRational 0 (3 % 4), LispRational 0 (7 % 9)]
    (return (LispRational 0 (7 % 12)))

lispAdditionTest1 :: Test
lispAdditionTest1 = lispMathsTest lispAddition
    [LispInteger 0 1, LispInteger 0 2, LispInteger 0 3]
    (return (LispInteger 0 6))

lispAdditionTest2 :: Test
lispAdditionTest2 = lispMathsTest lispAddition
    [LispInteger 0 1, LispInteger 0 2]
    (return (LispInteger 0 3))

lispAdditionTest3 :: Test
lispAdditionTest3 = lispMathsTest lispAddition
    [LispInteger 0 1]
    (return (LispInteger 0 1))

lispAdditionTest4 :: Test
lispAdditionTest4 = lispMathsTest lispAddition
    []
    (return (LispInteger 0 0))

lispAdditionTest5 :: Test
lispAdditionTest5 = lispMathsTest lispAddition
    [LispInteger 0 1234567890123456789, LispInteger 0 9876543210987654321]
    (return (LispInteger 0 11111111101111111110))

lispAdditionTest6 :: Test
lispAdditionTest6 = lispMathsTest lispAddition
    [LispReal 0 1.3, LispInteger 0 (-5)]
    (return (LispReal 0 (-3.7)))

lispAdditionTest7 :: Test
lispAdditionTest7 = lispMathsTest lispAddition
    [LispRational 0 (3 % 4), LispRational 0 (7 % 9)]
    (return (LispRational 0 (55 % 36)))

lispSubtractTest1 :: Test
lispSubtractTest1 = lispMathsTest lispSubtract
    [LispInteger 0 1, LispInteger 0 2, LispInteger 0 3]
    (return (LispInteger 0 (-4)))

lispSubtractTest2 :: Test
lispSubtractTest2 = lispMathsTest lispSubtract
    [LispInteger 0 1, LispInteger 0 2]
    (return (LispInteger 0 (-1)))

lispSubtractTest3 :: Test
lispSubtractTest3 = lispMathsTest lispSubtract
    [LispInteger 0 1]
    (return (LispInteger 0 (-1)))

lispSubtractTest4 :: Test
lispSubtractTest4 = lispMathsTest lispSubtract
    [LispInteger 0 1234567890123456789, LispInteger 0 9876543210987654321]
    (return (LispInteger 0 (-8641975320864197532)))

lispSubtractTest5 :: Test
lispSubtractTest5 = lispMathsTest lispSubtract
    [LispReal 0 1.3, LispInteger 0 (-5)]
    (return (LispReal 0 6.3))

lispSubtractTest6 :: Test
lispSubtractTest6 = lispMathsTest lispSubtract
    [LispRational 0 (3 % 4), LispRational 0 (7 % 9)]
    (return (LispRational 0 (- (1 % 36))))

lispDivisionTest1 :: Test
lispDivisionTest1 = lispMathsTest lispDivision
    [LispInteger 0 10]
    (return (LispRational 0 (1 % 10)))

lispDivisionTest2 :: Test
lispDivisionTest2 = lispMathsTest lispDivision
    [LispReal 0 10.0]
    (return (LispReal 0 0.1))

lispDivisionTest3 :: Test
lispDivisionTest3 = lispMathsTest lispDivision
    [LispInteger 0 10, LispInteger 0 2]
    (return (LispInteger 0 5))

lispDivisionTest4 :: Test
lispDivisionTest4 = lispMathsTest lispDivision
    [LispInteger 0 2, LispInteger 0 10]
    (return (LispRational 0 (1 % 5)))

lispDivisionTest5 :: Test
lispDivisionTest5 = lispMathsTest lispDivision
    [LispInteger 0 100, LispInteger 0 2, LispInteger 0 5, LispInteger 0 2]
    (return (LispInteger 0 5))

lispDivisionTest6 :: Test
lispDivisionTest6 = lispMathsTest lispDivision
    [LispInteger 0 1234567890123456789, LispInteger 0 9876543210987654321]
    (return (LispRational 0 (13717421 % 109739369)))

lispDivisionTest7 :: Test
lispDivisionTest7 = lispMathsTest lispDivision
    [LispReal 0 1.3, LispInteger 0 (-5)]
    (return (LispReal 0 (-0.26)))

lispDivisionTest8 :: Test
lispDivisionTest8 = lispMathsTest lispDivision
    [LispRational 0 (3 % 4), LispRational 0 (7 % 9)]
    (return (LispRational 0 (27 % 28)))
