{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module SyntaxAnalyserTest where

import SyntaxAnalyser (SyntaxAnalyserError(..), LispSyntax(..), syntaxAnalyse)
import Token (Token(..))
import Tokeniser (tokenise)

import Test.HUnit

syntaxAnalyserTest :: String -> Either SyntaxAnalyserError [LispSyntax] -> Test
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
        [ ExecList
            [ NumValue (Number 1 "1")
            , NumValue (Number 3 "2")
            , NumValue (Number 5 "3")
            ]
        , RawList
            [ VarValue (Identifier 10 "a")
            , VarValue (Identifier 12 "+")
            , StrValue (StringLiteral 18 "aaa")
            ]
        ]
    )

syntaxAnalyserTest2 :: Test
syntaxAnalyserTest2 = syntaxAnalyserTest
    "'((0) 1 2 (3 4 (5 6) 7) 8 (9))"
    ( Right
        [ RawList
            [ ExecList
                [ NumValue (Number 3 "0")
                ]
            , NumValue (Number 6 "1")
            , NumValue (Number 8 "2")
            , ExecList
                [ NumValue (Number 11 "3")
                , NumValue (Number 13 "4")
                , ExecList
                    [ NumValue (Number 16 "5")
                    , NumValue (Number 18 "6")
                    ]
                , NumValue (Number 21 "7")
                ]
            , NumValue (Number 24 "8")
            , ExecList
                [ NumValue (Number 27 "9")
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
        [ ExecList
            [ VarValue (Identifier 25 "defn")
            , VarValue (Identifier 35 "factorial")
            , ExecList
                [ VarValue (Identifier 38 "n")
                ]
            , ExecList
                [ VarValue (Identifier 45 "if")
                , ExecList
                    [ VarValue (Identifier 48 "=")
                    , VarValue (Identifier 50 "n")
                    , NumValue (Number 52 "1")
                    ]
                , NumValue (Number 59 "1")
                , ExecList
                    [ VarValue (Identifier 66 "*")
                    , VarValue (Identifier 68 "n")
                    , ExecList
                        [ VarValue (Identifier 79 "factorial")
                        , ExecList
                            [ VarValue (Identifier 82 "-")
                            , VarValue (Identifier 84 "n")
                            , NumValue (Number 86 "1")
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
