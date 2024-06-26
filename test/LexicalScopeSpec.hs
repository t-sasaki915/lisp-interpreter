{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LexicalScopeSpec where

import LispInterpreter (interpretLisp, initEnv)
import LispSystem
import Parser (parse)

import Control.Monad.Trans.Except (withExceptT, runExceptT, runExcept, except)
import Control.Monad.Trans.State.Strict (runStateT)
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit (Test(TestCase), assertEqual)

lexicalScopeTest :: String -> LispData -> Test
lexicalScopeTest src e =
    let
        program =
            withExceptT show (except $ runExcept $ parse src) >>=
                (withExceptT show . interpretLisp)

        result =
            fst $ unsafePerformIO $ runStateT (runExceptT program) initEnv
    in
    case result of
        Right d ->
            TestCase $ assertEqual "" d e

        Left err ->
            TestCase $ assertEqual "" err ""

{-
  The reference of test codes is
  https://ja.wikipedia.org/wiki/%E9%9D%99%E7%9A%84%E3%82%B9%E3%82%B3%E3%83%BC%E3%83%97
-}

lexicalScopeTest1 :: Test
lexicalScopeTest1 = lexicalScopeTest
    (unlines
        [ "(define *a* 0)"
        , "(set! *a* 5)"
        , "*a*"
        ]
    )
    (LispInteger 5)

lexicalScopeTest2 :: Test
lexicalScopeTest2 = lexicalScopeTest
    (unlines
        [ "(define *a* 0)"
        , "(set! *a* 5)"
        , "(let ((*a* 3)) *a*)"
        ]
    )
    (LispInteger 3)

lexicalScopeTest3 :: Test
lexicalScopeTest3 = lexicalScopeTest
    (unlines
        [ "(define *a* 0)"
        , "(set! *a* 5)"
        , "(let ((*a* 3)) *a*)"
        , "*a*"
        ]
    )
    (LispInteger 5)

lexicalScopeTest4 :: Test
lexicalScopeTest4 = lexicalScopeTest
    (unlines
        [ "(define func-lex (let ((a 3)) (lambda () a)))"
        , "(let ((a 5)) (func-lex))"
        ]
    )
    (LispInteger 3)

lexicalScopeTest5 :: Test
lexicalScopeTest5 = lexicalScopeTest
    (unlines
        [ "(define *a* 5)"
        , "(define func-dyn (let ((*a* 3)) (lambda () *a*)))"
        , "(func-dyn)"
        ]
    )
    (LispInteger 5)
