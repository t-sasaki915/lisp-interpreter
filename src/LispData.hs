{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispData where

import Data.Ratio ((%), numerator, denominator)

data LispData = LispInteger Int Integer
              | LispReal Int Float
              | LispRational Int Rational
              | LispSymbol Int String
              | LispBool Int Bool
              | LispString Int String
              | LispCharacter Int Char
              | LispList Int [LispData]
              | LispPair Int (LispData, LispData)
              | LispQuote LispData
              deriving Eq

data LispNumber = LispInteger' Integer
                | LispReal' Float
                | LispRational' Rational

toReal :: LispNumber -> Float
toReal (LispInteger' n)  = fromIntegral n
toReal (LispReal' r)     = r
toReal (LispRational' r) = fromIntegral (numerator r) / fromIntegral (denominator r)

index :: LispData -> Int
index (LispInteger n _)   = n
index (LispReal n _)      = n
index (LispRational n _)  = n
index (LispSymbol n _)    = n
index (LispBool n _)      = n
index (LispString n _)    = n
index (LispCharacter n _) = n
index (LispList n _)      = n
index (LispPair n _)      = n
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
lispType (LispPair _ _)      = "PAIR"
lispType (LispQuote d)       = "'" ++ lispType d

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
    show (LispPair _ p)      = "(" ++ show (fst p) ++ " . " ++ show (snd p) ++ ")"
    show (LispQuote d)       = "'" ++ show d

instance Eq LispNumber where
    (==) (LispInteger' z1) (LispInteger' z2) =
        z1 == z2
    (==) (LispInteger' z1) (LispReal' r1) =
        fromIntegral z1 == r1
    (==) (LispInteger' z1) (LispRational' r1) =
        (z1 % 1) == r1
    (==) (LispReal' r1) (LispInteger' z1) =
        r1 == fromIntegral z1
    (==) (LispReal' r1) (LispReal' r2) =
        r1 == r2
    (==) (LispReal' r1) (LispRational' r2) =
        r1 == (fromIntegral (numerator r2) / fromIntegral (denominator r2))
    (==) (LispRational' r1) (LispInteger' z1) =
        r1 == (z1 % 1)
    (==) (LispRational' r1) (LispReal' r2) =
        (fromIntegral (numerator r1) / fromIntegral (denominator r1)) == r2
    (==) (LispRational' r1) (LispRational' r2) =
        r1 == r2

instance Ord LispNumber where
    (<=) (LispInteger' z1) (LispInteger' z2) =
        z1 <= z2
    (<=) (LispInteger' z1) (LispReal' r1) =
        fromIntegral z1 <= r1
    (<=) (LispInteger' z1) (LispRational' r1) =
        (z1 % 1) <= r1
    (<=) (LispReal' r1) (LispInteger' z1) =
        r1 <= fromIntegral z1
    (<=) (LispReal' r1) (LispReal' r2) =
        r1 <= r2
    (<=) (LispReal' r1) (LispRational' r2) =
        r1 <= (fromIntegral (numerator r2) / fromIntegral (denominator r2))
    (<=) (LispRational' r1) (LispInteger' z1) =
        r1 <= (z1 % 1)
    (<=) (LispRational' r1) (LispReal' r2) =
        (fromIntegral (numerator r1) / fromIntegral (denominator r1)) <= r2
    (<=) (LispRational' r1) (LispRational' r2) =
        r1 <= r2

instance Num LispNumber where
    (+) (LispReal' r1) (LispReal' r2) =
        LispReal' (r1 + r2)
    (+) (LispReal' r1) (LispRational' r2) =
        LispReal' (r1 + (fromIntegral (numerator r2) / fromIntegral (denominator r2)))
    (+) (LispReal' r1) (LispInteger' n1) =
        LispReal' (r1 + fromIntegral n1)
    (+) (LispRational' r1) (LispReal' r2) =
        LispReal' ((fromIntegral (numerator r1) / fromIntegral (denominator r1)) + r2)
    (+) (LispRational' r1) (LispRational' r2) =
        LispRational' (r1 + r2)
    (+) (LispRational' r1) (LispInteger' n1) =
        LispRational' (r1 + (n1 % 1)) 
    (+) (LispInteger' n1) (LispReal' r1) =
        LispReal' (fromIntegral n1 + r1)
    (+) (LispInteger' n1) (LispRational' r1) =
        LispRational' ((n1 % 1) + r1)
    (+) (LispInteger' n1) (LispInteger' n2) =
        LispInteger' (n1 + n2)

    (*) (LispReal' r1) (LispReal' r2) =
        LispReal' (r1 * r2)
    (*) (LispReal' r1) (LispRational' r2) =
        LispReal' (r1 * (fromIntegral (numerator r2) / fromIntegral (denominator r2)))
    (*) (LispReal' r1) (LispInteger' n1) =
        LispReal' (r1 * fromIntegral n1)
    (*) (LispRational' r1) (LispReal' r2) =
        LispReal' ((fromIntegral (numerator r1) / fromIntegral (denominator r1)) * r2)
    (*) (LispRational' r1) (LispRational' r2) =
        LispRational' (r1 * r2)
    (*) (LispRational' r1) (LispInteger' n1) =
        LispRational' (r1 * (n1 % 1)) 
    (*) (LispInteger' n1) (LispReal' r1) =
        LispReal' (fromIntegral n1 * r1)
    (*) (LispInteger' n1) (LispRational' r1) =
        LispRational' ((n1 % 1) * r1)
    (*) (LispInteger' n1) (LispInteger' n2) =
        LispInteger' (n1 * n2)

    abs (LispReal' r)     = LispReal' (abs r)
    abs (LispRational' r) = LispRational' (abs r)
    abs (LispInteger' n)  = LispInteger' (abs n)

    negate (LispReal' r)     = LispReal' (negate r)
    negate (LispRational' r) = LispRational' (negate r)
    negate (LispInteger' n)  = LispInteger' (negate n)

    signum (LispReal' r)     = LispReal' (signum r)
    signum (LispRational' r) = LispRational' (signum r)
    signum (LispInteger' n)  = LispInteger' (signum n)

    fromInteger = LispInteger'

instance Fractional LispNumber where
    (/) (LispReal' r1) (LispReal' r2) =
        LispReal' (r1 / r2)
    (/) (LispReal' r1) (LispRational' r2) =
        LispReal' (r1 / (fromIntegral (numerator r2) / fromIntegral (denominator r2)))
    (/) (LispReal' r1) (LispInteger' n1) =
        LispReal' (r1 / fromIntegral n1)
    (/) (LispRational' r1) (LispReal' r2) =
        LispReal' ((fromIntegral (numerator r1) / fromIntegral (denominator r1)) / r2)
    (/) (LispRational' r1) (LispRational' r2) =
        LispRational' (r1 / r2)
    (/) (LispRational' r1) (LispInteger' n1) =
        LispRational' (r1 / (n1 % 1)) 
    (/) (LispInteger' n1) (LispReal' r1) =
        LispReal' (fromIntegral n1 / r1)
    (/) (LispInteger' n1) (LispRational' r1) =
        LispRational' ((n1 % 1) / r1)
    (/) (LispInteger' n1) (LispInteger' n2) =
        LispRational' (n1 % n2)

    fromRational = LispRational'
