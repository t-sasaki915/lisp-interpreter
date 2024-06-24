{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispDataExtra where

import LispData (LispData(..), LispNumber(..), indAndType)
import LispEnv (EvalT)
import LispError (RuntimeError(..))

import Control.Monad.Trans.Except (throwE)
import Data.Ratio ((%), numerator, denominator)
import Numeric (readFloat)

fromLispNumber :: Int -> LispNumber -> EvalT LispData
fromLispNumber n (LispInteger' z)  = return (LispInteger n z)
fromLispNumber n (LispReal' r)     = return (LispReal n r)
fromLispNumber n (LispRational' r) = case denominator r of
    1 -> return (LispInteger n (numerator r))
    _ -> return (LispRational n r)

treatAsLispBool :: LispData -> EvalT Bool
treatAsLispBool (LispList _ [])    = return False
treatAsLispBool (LispBool _ False) = return False
treatAsLispBool _                  = return True

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

treatAsLispNumber :: LispData -> EvalT LispNumber
treatAsLispNumber (LispInteger _ n)  = return (LispInteger' n)
treatAsLispNumber (LispRational _ r) = return (LispRational' r)
treatAsLispNumber (LispReal _ r)     = return (LispReal' r)
treatAsLispNumber d = throwE (uncurry IncompatibleType (indAndType d) "NUMBER")
