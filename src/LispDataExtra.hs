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

treatAsLispReals :: [LispData] -> EvalT [Float]
treatAsLispReals = mapM unwrap
    where unwrap = \case
            (LispReal _ r)     ->
                return r
            (LispRational _ r) ->
                return (fromIntegral (numerator r) / fromIntegral (denominator r))
            (LispInteger _ n)  ->
                return (fromIntegral n)
            d ->
                throwE (uncurry IncompatibleType (indAndType d) "NUMBER")

treatAsLispRats :: [LispData] -> EvalT [Rational]
treatAsLispRats = mapM unwrap
    where unwrap = \case
            (LispReal _ r) ->
                return (fst . head $ readFloat (show r))
            (LispRational _ r) ->
                return r
            (LispInteger _ n) ->
                return (n % 1)
            d ->
                throwE (uncurry IncompatibleType (indAndType d) "NUMBER")

treatAsLispInts :: [LispData] -> EvalT [Integer]
treatAsLispInts = mapM unwrap
    where unwrap = \case
            (LispInteger _ n) ->
                return n
            d ->
                throwE (uncurry IncompatibleType (indAndType d) "NUMBER")

treatAsLispReal :: LispData -> EvalT Float
treatAsLispReal d = treatAsLispReals [d] <&> head
