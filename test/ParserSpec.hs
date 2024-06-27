{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ParserSpec where

import LispError (ParseError(..))
import LispSystem (LispData(..))
import Parser (parse)

import Control.Monad.Trans.Except (Except, runExcept)
import Data.Ratio ((%))
import Test.HUnit (Test(TestCase), assertEqual)

parserTest :: String -> Except ParseError [LispData] -> Test
parserTest src e =
    TestCase $ assertEqual "" (runExcept $ parse src) (runExcept e)

parserTest1 :: Test
parserTest1 = parserTest
    "(abc def)"
    ( return
            [ LispList
                [ LispSymbol "ABC"
                , LispSymbol "DEF"
                ]
            ]
    )

parserTest2 :: Test
parserTest2 = parserTest
    "(abc def) (1 2 3 4) ()"
    ( return
        [ LispList
            [ LispSymbol "ABC"
            , LispSymbol "DEF"
            ]
        , LispList
            [ LispInteger 1
            , LispInteger 2
            , LispInteger 3
            , LispInteger 4
            ]
        , LispList
            [
            ]
        ]
    )

parserTest3 :: Test
parserTest3 = parserTest
    "(aa 1 2.0 3/4 #\\a #\\Return t nil \"hello\")"
    ( return
        [ LispList
            [ LispSymbol "AA"
            , LispInteger 1
            , LispReal 2.0
            , LispRational (3 % 4)
            , LispCharacter 'a'
            , LispCharacter '\r'
            , LispBool True
            , LispBool False
            , LispString "hello"
            ]
        ]
    )

parserTest4 :: Test
parserTest4 = parserTest
    "(((1) 2 3) 4 (5) 6 (7 (8)) 9 0)"
    ( return
        [ LispList
            [ LispList
                [ LispList
                    [ LispInteger 1
                    ]
                , LispInteger 2
                , LispInteger 3
                ]
            , LispInteger 4
            , LispList
                [ LispInteger 5
                ]
            , LispInteger 6
            , LispList
                [ LispInteger 7
                , LispList
                    [ LispInteger 8
                    ]
                ]
            , LispInteger 9
            , LispInteger 0
            ]
        ]
    )

parserTest5 :: Test
parserTest5 = parserTest
    ( unlines
        [ ";   AAA"
        , ";;  BBB"
        , ";;; CCC"
        , "(1 2)"
        , ";;; DDD"
        , "(3 4)"
        ]
    )
    ( return
        [ LispList
            [ LispInteger 1
            , LispInteger 2
            ]
        , LispList
            [ LispInteger 3
            , LispInteger 4
            ]
        ]
    )

parserTest6 :: Test
parserTest6 = parserTest
    "(aa'aa) ''(a) '('1.0 '2 '16/20)"
    ( return
        [ LispList
            [ LispSymbol "AA"
            , LispQuote (LispSymbol "AA")
            ]
        , LispQuote (
            LispQuote (
                LispList
                    [ LispSymbol "A"
                    ]
            )
          )
        , LispQuote (
            LispList
                [ LispQuote (LispReal 1.0)
                , LispQuote (LispInteger 2)
                , LispQuote (LispRational (16 % 20))
                ]
          )
        ]
    )
