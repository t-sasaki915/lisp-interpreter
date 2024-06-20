{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module EvalSpec where

import Eval (eval)
import LispData (LispData(..))
import LispEnv (Eval, initEnv)
import LispError (RuntimeError(..))
import Parser (parse)

import Control.Monad.Trans.Except (runExcept, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (runStateT)
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit (Test(TestCase), assertEqual)

evalTest :: String -> Eval -> Test
evalTest src e =
    case runExcept $ parse src of
        Right prog -> let
            result =
                fmap head $ fst $ unsafePerformIO $
                    runStateT (runExceptT $ mapM eval prog) initEnv
            e' =
                fst $ unsafePerformIO $
                    runStateT (runExceptT e) initEnv
            in
            TestCase $ assertEqual "" result e'

        Left _ ->
            TestCase $ assertEqual "" "Parse" "Failure"

evalTest1 :: Test
evalTest1 = evalTest
    "(if #t 1 2)"
    (return (LispInteger 7 1))

evalTest2 :: Test
evalTest2 = evalTest
    "(if (if #t #f #t) (if (if 1 1 0) \"a\" #\\b) 0)"
    (return (LispInteger 42 0))

evalTest3 :: Test
evalTest3 = evalTest
    "(if #f 1)"
    (return (LispBool 2 False))

evalTest4 :: Test
evalTest4 = evalTest
    "(if #f)"
    (throwE (SyntaxError 2 "IF"))

evalTest5 :: Test
evalTest5 = evalTest
    "(if #T #T #T #T)"
    (throwE (SyntaxError 2 "IF"))

evalTest6 :: Test
evalTest6 = evalTest
    "(quote a)"
    (return (LispSymbol 7 "A"))

evalTest7 :: Test
evalTest7 = evalTest
    "(quote 'a)"
    (return (LispQuote (LispSymbol 8 "A")))

evalTest8 :: Test
evalTest8 = evalTest
    "(quote 'a 'a)"
    (throwE (SyntaxError 5 "QUOTE"))

evalTest9 :: Test
evalTest9 = evalTest
    "(quote)"
    (throwE (SyntaxError 5 "QUOTE"))