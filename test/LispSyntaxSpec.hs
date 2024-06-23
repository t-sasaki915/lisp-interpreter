{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispSyntaxSpec where

import LispData (LispData(..))
import LispEnv (Evalable, Eval)
import LispInterpreter (initEnv)
import LispSyntax
import SpecUtil

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Strict (runStateT)
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit (Test(TestCase), assertEqual)

lispSyntaxTest :: Evalable -> [LispData] -> Eval -> Test
lispSyntaxTest f args e =
    let
        result =
            fst $ unsafePerformIO $ runStateT (runExceptT (f 0 args)) initEnv
        e' =
            fst $ unsafePerformIO $ runStateT (runExceptT e) initEnv
    in
    TestCase $ assertEqual "" result e'

lispIFTest1 :: Test
lispIFTest1 = lispSyntaxTest lispIF
    [lispList [symbol ">", integer 3, integer 4], string "variant 1", string "variant 2"]
    (return (string "variant 2"))

lispIFTest2 :: Test
lispIFTest2 = lispSyntaxTest lispIF
    [lispList [symbol ">", integer 4, integer 3], string "variant 1", string "variant 2"]
    (return (string "variant 1"))

lispQUOTETest1 :: Test
lispQUOTETest1 = lispSyntaxTest lispQUOTE
    [lispList [symbol "+", integer 1, integer 2, integer 3]]
    (return (lispList [symbol "+", integer 1, integer 2, integer 3]))
