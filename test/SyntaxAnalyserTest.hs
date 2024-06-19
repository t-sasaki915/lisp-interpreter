{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module SyntaxAnalyserTest where

import Syntax (Syntax(..))
import SyntaxAnalyser (SyntaxAnalyserError(..), syntaxAnalyse)
import Tokeniser (tokenise)

import Control.Monad.Trans.Except (Except, throwE, runExcept)
import Test.HUnit (Test(TestCase), assertEqual)

syntaxAnalyserTest :: String -> Except SyntaxAnalyserError [Syntax] -> Test
syntaxAnalyserTest src res =
    case runExcept $ tokenise src of
        Right tokens ->
            TestCase $ assertEqual "" (syntaxAnalyse tokens) res

        Left _ ->
            TestCase $ assertEqual "" True False

syntaxAnalyserTest1 :: Test
syntaxAnalyserTest1 = syntaxAnalyserTest
    ( unlines
        [ "(1 2 3)"
        , "'(a + \"aaa\")"
        ]
    )
    ( return
        [ InstantList 6
            [ NumberRef 1 1
            , NumberRef 3 2
            , NumberRef 5 3
            ]
        , LazyList 19
            [ IdentifierRef 10 "a"
            , IdentifierRef 12 "+"
            , StringRef 18 "aaa"
            ]
        ]
    )

syntaxAnalyserTest2 :: Test
syntaxAnalyserTest2 = syntaxAnalyserTest
    "'((0) 1 2 (3 4 (5 6) 7) 8 (9))"
    ( return
        [ LazyList 29
            [ InstantList 4
                [ NumberRef 3 0
                ]
            , NumberRef 6 1
            , NumberRef 8 2
            , InstantList 22
                [ NumberRef 11 3
                , NumberRef 13 4
                , InstantList 19
                    [ NumberRef 16 5
                    , NumberRef 18 6
                    ]
                , NumberRef 21 7
                ]
            , NumberRef 24 8
            , InstantList 28
                [ NumberRef 27 9
                ]
            ]
        ]
    )

syntaxAnalyserTest3 :: Test
syntaxAnalyserTest3 = syntaxAnalyserTest
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
    ( return
        [ InstantList 95
            [ IdentifierRef 25 "defn"
            , IdentifierRef 35 "factorial"
            , InstantList 39
                [ IdentifierRef 38 "n"
                ]
            , InstantList 93
                [ IdentifierRef 45 "if"
                , InstantList 53
                    [ IdentifierRef 48 "="
                    , IdentifierRef 50 "n"
                    , NumberRef 52 1
                    ]
                , NumberRef 59 1
                , InstantList 89
                    [ IdentifierRef 66 "*"
                    , IdentifierRef 68 "n"
                    , InstantList 88
                        [ IdentifierRef 79 "factorial"
                        , InstantList 87
                            [ IdentifierRef 82 "-"
                            , IdentifierRef 84 "n"
                            , NumberRef 86 1
                            ]
                        ]
                    ]
                ]
            ]
        ]
    )

syntaxAnalyserTest4 :: Test
syntaxAnalyserTest4 = syntaxAnalyserTest
    "(1))"
    (throwE $ UnexpectedToken 3 "')'" "'(' or '''")

syntaxAnalyserTest5 :: Test
syntaxAnalyserTest5 = syntaxAnalyserTest
    "(1 "
    (throwE $ UnexpectedEOF 0)
