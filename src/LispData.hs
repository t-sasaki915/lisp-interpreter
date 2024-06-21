{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispData(LispData(..), index, lispType, indAndType) where

import Data.Ratio (numerator, denominator)

data LispData = LispInteger Int Integer
              | LispReal Int Float
              | LispRational Int Rational
              | LispSymbol Int String
              | LispBool Int Bool
              | LispString Int String
              | LispCharacter Int Char
              | LispList Int [LispData]
              | LispQuote LispData
              deriving Eq

index :: LispData -> Int
index (LispInteger n _)   = n
index (LispReal n _)      = n
index (LispRational n _)  = n
index (LispSymbol n _)    = n
index (LispBool n _)      = n
index (LispString n _)    = n
index (LispCharacter n _) = n
index (LispList n _)      = n
index (LispQuote d)       = index d

lispType :: LispData -> String
lispType (LispInteger _ _)   = "INT"
lispType (LispReal _ _)      = "REAL"
lispType (LispRational _ _)  = "RATIONAL"
lispType (LispSymbol _ _)    = "SYMBOL"
lispType (LispBool _ _)      = "BOOL"
lispType (LispString _ _)    = "STRING"
lispType (LispCharacter _ _) = "CHAR"
lispType (LispList _ _)      = "LIST"
lispType (LispQuote d)       = lispType d

indAndType :: LispData -> (Int, String)
indAndType d = (index d, lispType d)

instance Show LispData where
    show (LispInteger _ n)   = show n
    show (LispReal _ n)      = show n
    show (LispRational _ a)  = show (numerator a) ++ "/" ++ show (denominator a)
    show (LispSymbol _ n)    = n
    show (LispBool _ True)   = "#T"
    show (LispBool _ False)  = "#F"
    show (LispString _ s)    = "\"" ++ s ++ "\""
    show (LispCharacter _ c) = "#\\" ++ [c]
    show (LispList _ l)      = "(" ++ unwords (map show l) ++ ")"
    show (LispQuote d)       = "'" ++ show d
