{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}

module LispDataExtra where

import LispData (LispData(..), indAndType)
import LispEnv (EvalT)
import LispError (RuntimeError(..))

import Control.Monad.Trans.Except (throwE)
import Data.Functor ((<&>))
import Data.List (find)
import Data.Maybe (isJust)
import Data.Ratio ((%), numerator, denominator)
import Numeric (readFloat)

treatAsLispBool :: LispData -> EvalT Bool
treatAsLispBool (LispList _ [])    = return False
treatAsLispBool (LispBool _ False) = return False
treatAsLispBool _                  = return True

isThereReal :: [LispData] -> Bool
isThereReal = isJust . find match
    where match = \case (LispReal _ _) -> True
                        _              -> False

isThereRat :: [LispData] -> Bool
isThereRat = isJust . find match
    where match = \case (LispRational _ _) -> True
                        _                  -> False

treatAsLispReal :: LispData -> EvalT Float
treatAsLispReal (LispReal _ r)     = return r
treatAsLispReal (LispRational _ r) =
    return (fromIntegral (numerator r) / fromIntegral (denominator r))
treatAsLispReal (LispInteger _ n)  = return (fromIntegral n)
treatAsLispReal d = throwE (uncurry IncompatibleType (indAndType d) "NUMBER")

treatAsLispRat :: LispData -> EvalT Rational
treatAsLispRat (LispReal _ r)     = return (fst . head $ readFloat (show r))
treatAsLispRat (LispRational _ r) = return r
treatAsLispRat (LispInteger _ n)  = return (n % 1)
treatAsLispRat d = throwE (uncurry IncompatibleType (indAndType d) "NUMBER")

treatAsLispInt :: LispData -> EvalT Integer
treatAsLispInt (LispInteger _ n) = return n
treatAsLispInt d = throwE (uncurry IncompatibleType (indAndType d) "INT")

treatAsLispReals :: [LispData] -> EvalT [Float]
treatAsLispReals = mapM treatAsLispReal

treatAsLispRats :: [LispData] -> EvalT [Rational]
treatAsLispRats = mapM treatAsLispRat

treatAsLispInts :: [LispData] -> EvalT [Integer]
treatAsLispInts = mapM treatAsLispInt
