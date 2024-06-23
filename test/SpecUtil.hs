{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module SpecUtil where

import LispData (LispData(..))

integer :: Integer -> LispData
integer = LispInteger 0

real :: Float -> LispData
real = LispReal 0

rational :: Rational -> LispData
rational = LispRational 0

boolean :: Bool -> LispData
boolean = LispBool 0

lispList :: [LispData] -> LispData
lispList = LispList 0

symbol :: String -> LispData
symbol = LispSymbol 0

string :: String -> LispData
string = LispString 0
