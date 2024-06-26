{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispSyntaxSpec where

import LispSyntax
import LispSpec (lispProcedureTest)
import LispSystem (LispData(..))

import Test.HUnit (Test)

lispIFTest1 :: Test
lispIFTest1 = lispProcedureTest lispIF
    [ LispList [LispSymbol ">", LispInteger 3, LispInteger 4]
    , LispString "variant 1"
    , LispString "variant 2"
    ]
    (return (LispString "variant 2"))

lispIFTest2 :: Test
lispIFTest2 = lispProcedureTest lispIF
    [ LispList [ LispSymbol ">", LispInteger 4, LispInteger 3]
    , LispString "variant 1"
    , LispString "variant 2"
    ]
    (return (LispString "variant 1"))

lispQUOTETest1 :: Test
lispQUOTETest1 = lispProcedureTest lispQUOTE
    [LispList [LispSymbol "+", LispInteger 1, LispInteger 2, LispInteger 3]]
    (return (LispList [LispSymbol "+", LispInteger 1, LispInteger 2, LispInteger 3]))

lispBEGINTest1 :: Test
lispBEGINTest1 = lispProcedureTest lispBEGIN
    [LispInteger 1, LispInteger 2, LispInteger 3, LispInteger 4, LispInteger 5]
    (return (LispInteger 5))

lispBEGINTest2 :: Test
lispBEGINTest2 = lispProcedureTest lispBEGIN
    [LispInteger 1, LispInteger 2, LispList [LispSymbol "SIN", LispReal 1.0]]
    (return (LispReal 0.84147096))

lispBEGINTest3 :: Test
lispBEGINTest3 = lispProcedureTest lispBEGIN
    []
    (return (LispBool False))
