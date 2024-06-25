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
            [ LispList 8
                [ LispSymbol 3 "ABC"
                , LispSymbol 7 "DEF"
                ]
            ]
    )

parserTest2 :: Test
parserTest2 = parserTest
    "(abc def) (1 2 3 4) ()"
    ( return
        [ LispList 8
            [ LispSymbol 3 "ABC"
            , LispSymbol 7 "DEF"
            ]
        , LispList 18
            [ LispInteger 11 1
            , LispInteger 13 2
            , LispInteger 15 3
            , LispInteger 17 4
            ]
        , LispList 21
            [
            ]
        ]
    )

parserTest3 :: Test
parserTest3 = parserTest
    "(aa 1 2.0 3/4 #\\a #\\Return #t #F \"hello\")"
    ( return
        [ LispList 40
            [ LispSymbol 2 "AA"
            , LispInteger 4 1
            , LispReal 8 2.0
            , LispRational 12 (3 % 4)
            , LispCharacter 16 'a'
            , LispCharacter 25 '\r'
            , LispBool 28 True
            , LispBool 31 False
            , LispString 39 "hello"
            ]
        ]
    )

parserTest4 :: Test
parserTest4 = parserTest
    "(((1) 2 3) 4 (5) 6 (7 (8)) 9 0)"
    ( return
        [ LispList 30
            [ LispList 9
                [ LispList 4
                    [ LispInteger 3 1
                    ]
                , LispInteger 6 2
                , LispInteger 8 3
                ]
            , LispInteger 11 4
            , LispList 15
                [ LispInteger 14 5
                ]
            , LispInteger 17 6
            , LispList 25
                [ LispInteger 20 7
                , LispList 24
                    [ LispInteger 23 8
                    ]
                ]
            , LispInteger 27 9
            , LispInteger 29 0
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
        [ LispList 28
            [ LispInteger 25 1
            , LispInteger 27 2
            ]
        , LispList 42
            [ LispInteger 39 3
            , LispInteger 41 4
            ]
        ]
    )

parserTest6 :: Test
parserTest6 = parserTest
    "(aa'aa) ''(a) '('1.0 '2 '16/20)"
    ( return
        [ LispList 6
            [ LispSymbol 2 "AA"
            , LispQuote (LispSymbol 5 "AA")
            ]
        , LispQuote (
            LispQuote (
                LispList 12
                    [ LispSymbol 11 "A"
                    ]
            )
          )
        , LispQuote (
            LispList 30
                [ LispQuote (LispReal 19 1.0)
                , LispQuote (LispInteger 22 2)
                , LispQuote (LispRational 29 (16 % 20))
                ]
          )
        ]
    )
