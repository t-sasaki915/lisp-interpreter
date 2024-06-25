{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE FlexibleInstances #-}

module LispSystem where

import LispError (RuntimeError)

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State.Strict (StateT)
import Data.Ratio ((%), numerator, denominator)

type Execution a = ExceptT RuntimeError (StateT LispEnv IO) a

type Procedure = Int -> [LispData] -> Execution LispData

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
              | LispClosure Int [(String, LispEnvData)] LispData
              deriving Eq

data LispEnvData = LispFunction Procedure
                 | LispSyntax Procedure
                 | LispVariable LispData
                 | LispVariableBind
                 deriving Eq

data LispEnv = LispEnv
    { global  :: [(String, LispEnvData)]
    , lexical :: [(String, LispEnvData)]
    }
    deriving Eq

data LispNumber = LispInteger' Integer
                | LispReal' Float
                | LispRational' Rational

instance Eq Procedure where
    (==) _ _ = True

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
    show (LispClosure {})    = "CLOSURE"

instance Eq LispNumber where
    (==) (LispInteger' z1) (LispInteger' z2) =
        z1 == z2
    (==) (LispInteger' z1) (LispReal' r1) =
        fromIntegral z1 == r1
    (==) (LispInteger' z1) (LispRational' r1) =
        fromIntegral z1 == r1
    (==) (LispReal' r1) (LispInteger' z1) =
        r1 == fromIntegral z1
    (==) (LispReal' r1) (LispReal' r2) =
        r1 == r2
    (==) (LispReal' r1) (LispRational' r2) =
        r1 == fromRational r2
    (==) (LispRational' r1) (LispInteger' z1) =
        r1 == fromIntegral z1
    (==) (LispRational' r1) (LispReal' r2) =
        fromRational r1 == r2
    (==) (LispRational' r1) (LispRational' r2) =
        r1 == r2

instance Ord LispNumber where
    (<=) (LispInteger' z1) (LispInteger' z2) =
        z1 <= z2
    (<=) (LispInteger' z1) (LispReal' r1) =
        fromIntegral z1 <= r1
    (<=) (LispInteger' z1) (LispRational' r1) =
        fromIntegral z1 <= r1
    (<=) (LispReal' r1) (LispInteger' z1) =
        r1 <= fromIntegral z1
    (<=) (LispReal' r1) (LispReal' r2) =
        r1 <= r2
    (<=) (LispReal' r1) (LispRational' r2) =
        r1 <= fromRational r2
    (<=) (LispRational' r1) (LispInteger' z1) =
        r1 <= fromIntegral z1
    (<=) (LispRational' r1) (LispReal' r2) =
        fromRational r1 <= r2
    (<=) (LispRational' r1) (LispRational' r2) =
        r1 <= r2

instance Num LispNumber where
    (+) (LispReal' r1) (LispReal' r2) =
        LispReal' (r1 + r2)
    (+) (LispReal' r1) (LispRational' r2) =
        LispReal' (r1 + fromRational r2)
    (+) (LispReal' r1) (LispInteger' n1) =
        LispReal' (r1 + fromIntegral n1)
    (+) (LispRational' r1) (LispReal' r2) =
        LispReal' (fromRational r1 + r2)
    (+) (LispRational' r1) (LispRational' r2) =
        LispRational' (r1 + r2)
    (+) (LispRational' r1) (LispInteger' n1) =
        LispRational' (r1 + fromIntegral n1) 
    (+) (LispInteger' n1) (LispReal' r1) =
        LispReal' (fromIntegral n1 + r1)
    (+) (LispInteger' n1) (LispRational' r1) =
        LispRational' (fromIntegral n1 + r1)
    (+) (LispInteger' n1) (LispInteger' n2) =
        LispInteger' (n1 + n2)

    (*) (LispReal' r1) (LispReal' r2) =
        LispReal' (r1 * r2)
    (*) (LispReal' r1) (LispRational' r2) =
        LispReal' (r1 * fromRational r2)
    (*) (LispReal' r1) (LispInteger' n1) =
        LispReal' (r1 * fromIntegral n1)
    (*) (LispRational' r1) (LispReal' r2) =
        LispReal' (fromRational r1 * r2)
    (*) (LispRational' r1) (LispRational' r2) =
        LispRational' (r1 * r2)
    (*) (LispRational' r1) (LispInteger' n1) =
        LispRational' (r1 * fromIntegral n1) 
    (*) (LispInteger' n1) (LispReal' r1) =
        LispReal' (fromIntegral n1 * r1)
    (*) (LispInteger' n1) (LispRational' r1) =
        LispRational' (fromIntegral n1 * r1)
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
        LispReal' (r1 / fromRational r2)
    (/) (LispReal' r1) (LispInteger' n1) =
        LispReal' (r1 / fromIntegral n1)
    (/) (LispRational' r1) (LispReal' r2) =
        LispReal' (fromRational r1 / r2)
    (/) (LispRational' r1) (LispRational' r2) =
        LispRational' (r1 / r2)
    (/) (LispRational' r1) (LispInteger' n1) =
        LispRational' (r1 / fromIntegral n1) 
    (/) (LispInteger' n1) (LispReal' r1) =
        LispReal' (fromIntegral n1 / r1)
    (/) (LispInteger' n1) (LispRational' r1) =
        LispRational' (fromIntegral n1 / r1)
    (/) (LispInteger' n1) (LispInteger' n2) =
        LispRational' (n1 % n2)

    fromRational = LispRational'
