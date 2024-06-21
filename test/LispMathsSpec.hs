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
    [LispRational 0 (3 % 4), LispRational 0 (7 % 9)]
    (return (LispRational 0 (7 % 12)))
