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
