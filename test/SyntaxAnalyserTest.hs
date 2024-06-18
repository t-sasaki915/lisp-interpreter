{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module SyntaxAnalyserTest where

import Syntax (Syntax(..))
import SyntaxAnalyser (SyntaxAnalyserError(..), syntaxAnalyse)
import Tokeniser (tokenise)

import Test.HUnit

syntaxAnalyserTest :: String -> Either SyntaxAnalyserError [Syntax] -> Test
syntaxAnalyserTest src res =
    case tokenise src of
        Right tokens ->
            TestCase $ assertEqual "" (syntaxAnalyse src tokens) res

        Left _ ->
            TestCase $ assertEqual "" True False

syntaxAnalyserTest1 :: Test
syntaxAnalyserTest1 = syntaxAnalyserTest
    ( unlines
        [ "(1 2 3)"
        , "'(a + \"aaa\")"
        ]
    )
    ( Right
        [ InstantList 0
            [ NumberRef 1 1
            , NumberRef 3 2
            , NumberRef 5 3
            ]
        , LazyList 8
            [ IdentifierRef 10 "a"
            , IdentifierRef 12 "+"
            , StringRef 18 "aaa"
            ]
        ]
    )

syntaxAnalyserTest2 :: Test
syntaxAnalyserTest2 = syntaxAnalyserTest
    "'((0) 1 2 (3 4 (5 6) 7) 8 (9))"
    ( Right
        [ LazyList 0
            [ InstantList 2
                [ NumberRef 3 0
                ]
            , NumberRef 6 1
            , NumberRef 8 2
            , InstantList 10
                [ NumberRef 11 3
                , NumberRef 13 4
                , InstantList 15
                    [ NumberRef 16 5
                    , NumberRef 18 6
                    ]
                , NumberRef 21 7
                ]
            , NumberRef 24 8
            , InstantList 26
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
    ( Right
        [ InstantList 21
            [ IdentifierRef 25 "defn"
            , IdentifierRef 35 "factorial"
            , InstantList 37
                [ IdentifierRef 38 "n"
                ]
            , InstantList 43
                [ IdentifierRef 45 "if"
                , InstantList 47
                    [ IdentifierRef 48 "="
                    , IdentifierRef 50 "n"
                    , NumberRef 52 1
                    ]
                , NumberRef 59 1
                , InstantList 65
                    [ IdentifierRef 66 "*"
                    , IdentifierRef 68 "n"
                    , InstantList 70
                        [ IdentifierRef 79 "factorial"
                        , InstantList 81
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
    (Left $ UnexpectedToken "(1))" 3 "')'" "'(' or '''")

syntaxAnalyserTest5 :: Test
syntaxAnalyserTest5 = syntaxAnalyserTest
    "(1 "
    (Left $ UnexpectedEOF "(1 " 0)
