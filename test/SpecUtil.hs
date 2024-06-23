module SpecUtil (integer, real, rational, boolean) where

import LispData (LispData(..))

integer :: Integer -> LispData
integer = LispInteger 0
real :: Float -> LispData
real = LispReal 0
rational :: Rational -> LispData
rational = LispRational 0
boolean :: Bool -> LispData
boolean = LispBool 0
