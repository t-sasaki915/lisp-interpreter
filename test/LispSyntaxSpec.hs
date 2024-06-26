{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispSyntaxSpec where

import LispSyntax
import LispSpec

import Test.HUnit (Test)

lispIFTest1 :: Test
lispIFTest1 = lispProcedureTest lispIF
    [lispList [symbol ">", integer 3, integer 4], string "variant 1", string "variant 2"]
    (return (string "variant 2"))

lispIFTest2 :: Test
lispIFTest2 = lispProcedureTest lispIF
    [lispList [symbol ">", integer 4, integer 3], string "variant 1", string "variant 2"]
    (return (string "variant 1"))

lispQUOTETest1 :: Test
lispQUOTETest1 = lispProcedureTest lispQUOTE
    [lispList [symbol "+", integer 1, integer 2, integer 3]]
    (return (lispList [symbol "+", integer 1, integer 2, integer 3]))

lispBEGINTest1 :: Test
lispBEGINTest1 = lispProcedureTest lispBEGIN
    [integer 1, integer 2, integer 3, integer 4, integer 5]
    (return (integer 5))

lispBEGINTest2 :: Test
lispBEGINTest2 = lispProcedureTest lispBEGIN
    [integer 1, integer 2, lispList [symbol "SIN", real 1.0]]
    (return (real 0.84147096))

lispBEGINTest3 :: Test
lispBEGINTest3 = lispProcedureTest lispBEGIN
    []
    (return (boolean False))
