{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}

module LispMaths where

import LispData
import LispDataExtra
import LispEnv
import LispError (RuntimeError(..))
import Util (getM)

import Control.Monad.Trans.Except (throwE)
import Data.Functor ((<&>))

lispPredefMathsFunctions :: [(String, LispEnvData)]
lispPredefMathsFunctions =
    [ ("*",       LispProcedure lispMultiple)
    , ("+",       LispProcedure lispAddition)
    , ("-",       LispProcedure lispSubtract)
    , ("/",       LispProcedure lispDivision)
    , ("<",       LispProcedure lispLessThan)
    , ("<=",      LispProcedure lispLessThanOrEq)
    , ("=",       LispProcedure lispNumberEq)
    , (">",       LispProcedure lispGreaterThan)
    , (">=",      LispProcedure lispGreaterThanOrEq)
    , ("ABS",     LispProcedure lispABS)
    , ("COS",     LispProcedure lispCOS)
    , ("NOT",     LispProcedure lispNOT)
    , ("SIN",     LispProcedure lispSIN)
    , ("SQRT",    LispProcedure lispSQRT)
    , ("NUMBER?", LispProcedure lispNUMBERP)
    ]

guaranteeNotZero :: Int -> LispNumber -> EvalT LispNumber
guaranteeNotZero _ (LispInteger' n)  | n /= 0 = return (LispInteger' n)
guaranteeNotZero _ (LispRational' r) | r /= 0 = return (LispRational' r)
guaranteeNotZero _ (LispReal' r)     | r /= 0 = return (LispReal' r)
guaranteeNotZero n _ = throwE (ZeroDivideCalculation n)

lispMultiple :: Evalable
lispMultiple ind args
    | null args        = return (LispInteger ind 1)
    | length args == 1 = getM args 0
    | otherwise        =
        mapM treatAsLispNumber args >>=
            (fromLispNumber ind . product)

lispAddition :: Evalable
lispAddition ind args
    | null args        = return (LispInteger ind 0)
    | length args == 1 = getM args 0
    | otherwise        =
        mapM treatAsLispNumber args >>=
            (fromLispNumber ind . sum)

lispSubtract :: Evalable
lispSubtract ind args
    | null args        = throwE (TooFewArguments ind "-" 1)
    | length args == 1 =
        case head args of
            (LispReal _ r)     -> return (LispReal ind (negate r))
            (LispRational _ r) -> return (LispRational ind (negate r))
            (LispInteger _ n)  -> return (LispInteger ind (negate n))
            d -> throwE (uncurry IncompatibleType (indAndType d) "NUMBER")
    | otherwise        =
        mapM treatAsLispNumber args >>=
            (fromLispNumber ind . foldl1 (-))

lispDivision :: Evalable
lispDivision ind args
    | null args        = throwE (TooFewArguments ind "/" 1)
    | length args == 1 =
        treatAsLispNumber (head args) >>=
            guaranteeNotZero ind >>=
                (\a -> fromLispNumber ind (LispInteger' 1 / a))
    | otherwise        =
        mapM treatAsLispNumber args >>= (\nums ->
            mapM (guaranteeNotZero ind) (tail nums) >>=
                const (fromLispNumber ind (foldl1 (/) nums)))

lispLessThan :: Evalable
lispLessThan ind args
    | null args        = throwE (TooFewArguments ind "<" 1)
    | length args == 1 = return (LispBool ind True)
    | otherwise        = do
        vars <- mapM treatAsLispNumber args
        let res = foldl
                    (\case
                        False -> const False
                        True  -> \case
                            (0, _)   -> True
                            (n, var) -> (vars !! (n - 1)) < var
                    )
                    True
                    (zip [0..] vars)
        return (LispBool ind res)

lispLessThanOrEq :: Evalable
lispLessThanOrEq ind args
    | null args        = throwE (TooFewArguments ind "<=" 1)
    | length args == 1 = return (LispBool ind True)
    | otherwise        = do
        vars <- mapM treatAsLispNumber args
        let res = foldl
                    (\case
                        False -> const False
                        True  -> \case
                            (0, _)   -> True
                            (n, var) -> (vars !! (n - 1)) <= var
                    )
                    True
                    (zip [0..] vars)
        return (LispBool ind res)

lispNumberEq :: Evalable
lispNumberEq ind args
    | null args        = throwE (TooFewArguments ind "=" 1)
    | length args == 1 = return (LispBool ind True)
    | otherwise        = do
        vars <- mapM treatAsLispNumber args
        let res = foldl
                    (\case
                        False -> const False
                        True  -> \case
                            (0, _)   -> True
                            (n, var) -> (vars !! (n - 1)) == var
                    )
                    True
                    (zip [0..] vars)
        return (LispBool ind res)

lispGreaterThan :: Evalable
lispGreaterThan ind args
    | null args        = throwE (TooFewArguments ind ">" 1)
    | length args == 1 = return (LispBool ind True)
    | otherwise        = do
        vars <- mapM treatAsLispNumber args
        let res = foldl
                    (\case
                        False -> const False
                        True  -> \case
                            (0, _)   -> True
                            (n, var) -> (vars !! (n - 1)) > var
                    )
                    True
                    (zip [0..] vars)
        return (LispBool ind res)

lispGreaterThanOrEq :: Evalable
lispGreaterThanOrEq ind args
    | null args        = throwE (TooFewArguments ind ">=" 1)
    | length args == 1 = return (LispBool ind True)
    | otherwise        = do
        vars <- mapM treatAsLispNumber args
        let res = foldl
                    (\case
                        False -> const False
                        True  -> \case
                            (0, _)   -> True
                            (n, var) -> (vars !! (n - 1)) >= var
                    )
                    True
                    (zip [0..] vars)
        return (LispBool ind res)

lispABS :: Evalable
lispABS ind args
    | length args > 1 = throwE (TooManyArguments ind "ABS" 1)
    | null args       = throwE (TooFewArguments ind "ABS" 1)
    | otherwise       =
        treatAsLispNumber (head args) >>=
            (fromLispNumber ind . abs)

lispCOS :: Evalable
lispCOS ind args
    | length args > 1 = throwE (TooManyArguments ind "COS" 1)
    | null args       = throwE (TooFewArguments ind "COS" 1)
    | otherwise       =
        treatAsLispNumber (head args) >>=
            (fromLispNumber ind . LispReal' . cos . toReal)

lispNOT :: Evalable
lispNOT ind args
    | length args > 1 = throwE (TooManyArguments ind "NOT" 1)
    | null args       = throwE (TooFewArguments ind "NOT" 1)
    | otherwise       =
        treatAsLispBool (head args) <&> (LispBool ind . not)

lispSIN :: Evalable
lispSIN ind args
    | length args > 1 = throwE (TooManyArguments ind "SIN" 1)
    | null args       = throwE (TooFewArguments ind "SIN" 1)
    | otherwise       =
        treatAsLispNumber (head args) >>=
            (fromLispNumber ind . LispReal' . sin . toReal)

lispSQRT :: Evalable
lispSQRT ind args
    | length args > 1 = throwE (TooManyArguments ind "SQRT" 1)
    | null args       = throwE (TooFewArguments ind "SQRT" 1)
    | otherwise       =
        treatAsLispNumber (head args) >>=
            (fromLispNumber ind . LispReal' . sqrt . toReal)

lispNUMBERP :: Evalable
lispNUMBERP ind args
    | length args > 1 = throwE (TooManyArguments ind "NUMBER?" 1)
    | null args       = throwE (TooFewArguments ind "NUMBER?" 1)
    | otherwise       =
        case head args of
            (LispReal _ _)      -> return (LispBool ind True)
            (LispRational _ _ ) -> return (LispBool ind True)
            (LispInteger _ _)   -> return (LispBool ind True)
            _                   -> return (LispBool ind False)
