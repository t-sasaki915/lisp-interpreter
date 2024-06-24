{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispDataExtra where

import LispData (LispData(..), LispNumber(..), indAndType)
import LispEnv (EvalT)
import LispError (RuntimeError(..))

import Control.Monad.Trans.Except (throwE)
import Data.Ratio ((%), numerator, denominator)

fromLispNumber :: Int -> LispNumber -> EvalT LispData
fromLispNumber n (LispInteger' z)  = return (LispInteger n z)
fromLispNumber n (LispReal' r)     = return (LispReal n r)
fromLispNumber n (LispRational' r) = case (numerator r, denominator r) of
    (a, 1) -> return (LispInteger n a)
    (a, b) -> return (LispRational n (a % b))

treatAsLispBool :: LispData -> EvalT Bool
treatAsLispBool (LispList _ [])    = return False
treatAsLispBool (LispBool _ False) = return False
treatAsLispBool _                  = return True

treatAsLispNumber :: LispData -> EvalT LispNumber
treatAsLispNumber (LispInteger _ n)  = return (LispInteger' n)
treatAsLispNumber (LispRational _ r) = return (LispRational' r)
treatAsLispNumber (LispReal _ r)     = return (LispReal' r)
treatAsLispNumber d = throwE (uncurry IncompatibleType (indAndType d) "NUMBER")
