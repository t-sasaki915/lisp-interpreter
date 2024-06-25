{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispSpec where

import LispInterpreter (initEnv)
import LispSystem

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Strict (runStateT)
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit (Test(TestCase), assertEqual)

integer :: Integer -> LispData
integer = LispInteger 0

real :: Float -> LispData
real = LispReal 0

rational :: Rational -> LispData
rational = LispRational 0

boolean :: Bool -> LispData
boolean = LispBool 0

lispList :: [LispData] -> LispData
lispList = LispList 0

symbol :: String -> LispData
symbol = LispSymbol 0

string :: String -> LispData
string = LispString 0

lispProcedureTest :: Procedure -> [LispData] -> Execution LispData -> Test
lispProcedureTest f args e =
    let
        result =
            fst $ unsafePerformIO $ runStateT (runExceptT (f 0 args)) initEnv
        e' =
            fst $ unsafePerformIO $ runStateT (runExceptT e) initEnv
    in
    TestCase $ assertEqual "" result e'
