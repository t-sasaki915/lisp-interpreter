{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE FlexibleInstances #-}

module LispSystem where

import LispError (RuntimeError)

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State.Strict (StateT)
import Data.Ratio ((%), numerator, denominator)

type Execution a = ExceptT RuntimeError (StateT LispEnv IO) a

type Procedure = [LispData] -> Execution LispData

data LispData = LispInteger Integer
              | LispReal Float
              | LispRational Rational
              | LispSymbol String
              | LispBool Bool
              | LispString String
              | LispCharacter Char
              | LispList [LispData]
              | LispPair (LispData, LispData)
              | LispQuote LispData
              | LispClosure [(String, Maybe LispData)] [String] [LispData]
              deriving Eq

data LispEnvData = LispFunction Procedure
                 | LispSyntax Procedure
                 | LispUserFunction LispData
                 deriving Eq

data LispEnv = LispEnv
    { functions        :: [(String, LispEnvData)]
    , globalVariables  :: [(String, Maybe LispData)]
    , lexicalVariables :: [(String, Maybe LispData)]
    }
    deriving Eq

data LispNumber = LispInteger' Integer
                | LispReal' Float
                | LispRational' Rational

instance Eq Procedure where
    (==) _ _ = True

instance Show LispData where
    show (LispInteger n)   = show n
    show (LispReal n)      = show n
    show (LispRational a)  = show (numerator a) ++ "/" ++ show (denominator a)
    show (LispSymbol n)    = n
    show (LispBool True)   = "#T"
    show (LispBool False)  = "#F"
    show (LispString s)    = "\"" ++ s ++ "\""
    show (LispCharacter c) = "#\\" ++ [c]
    show (LispList l)      = "(" ++ unwords (map show l) ++ ")"
    show (LispPair p)      = "(" ++ show (fst p) ++ " . " ++ show (snd p) ++ ")"
    show (LispQuote d)     = "'" ++ show d
    show (LispClosure {})  = "CLOSURE"

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
