{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module TokeniserTest where

import Token (Token(..))
import Tokeniser (TokeniserError(..), tokenise)

import Test.HUnit

tokeniserTest :: String -> Either TokeniserError [Token] -> Test
tokeniserTest src res =
    TestCase $ assertEqual "" (tokenise src) res

tokeniserTest1 :: Test
tokeniserTest1 = tokeniserTest
    "(+ 1 2)"
    ( Right
        [ OpenParentheses 0
        , Identifier 1 "+"
        , Number 3 "1"
        , Number 5 "2"
        , CloseParentheses 6
        ]
    )

tokeniserTest2 :: Test
tokeniserTest2 = tokeniserTest
    ( unlines
        [ "; Factorial Function"
        , "(defn factorial (n)"
        , "  (if (= n 1)"
        , "    1"
        , "    (* n (factorial (- n 1)))"
        , "  )"
        , ")"
        ]
    )
    ( Right
        [ OpenParentheses 21
        , Identifier 25 "defn"
        , Identifier 35 "factorial"
        , OpenParentheses 37
        , Identifier 38 "n"
        , CloseParentheses 39
        , OpenParentheses 43
        , Identifier 45 "if"
        , OpenParentheses 47
        , Identifier 48 "="
        , Identifier 50 "n"
        , Number 52 "1"
        , CloseParentheses 53
        , Number 59 "1"
        , OpenParentheses 65
        , Identifier 66 "*"
        , Identifier 68 "n"
        , OpenParentheses 70
        , Identifier 79 "factorial"
        , OpenParentheses 81
        , Identifier 82 "-"
        , Identifier 84 "n"
        , Number 86 "1"
        , CloseParentheses 87
        , CloseParentheses 88
        , CloseParentheses 89
        , CloseParentheses 93
        , CloseParentheses 95
        ]
    )

tokeniserTest3 :: Test
tokeniserTest3 = tokeniserTest
    "(\"aaa)"
    (Left $ UnexpectedEOF 0)

tokeniserTest4 :: Test
tokeniserTest4 = tokeniserTest
    "'(1 2 3a)"
    (Left $ InvalidNumberFormat 7 "3a")
