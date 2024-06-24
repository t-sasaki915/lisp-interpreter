{-# LANGUAGE LambdaCase #-}

module Eval (eval) where

import LispData
import LispEnv
import LispError (RuntimeError(..))

import Control.Monad.Trans.Except (throwE)
import Data.Ratio ((%), numerator, denominator)

eval :: LispData -> Eval
eval = \case
    (LispSymbol n s) ->
        variableReference (LispSymbol n s)

    (LispQuote d) ->
        return d

    (LispInteger n z) ->
        return (LispInteger n z)

    (LispReal n r) ->
        return (LispReal n r)

    (LispRational n r) ->
        case (numerator r, denominator r) of
            (a, 1) -> return (LispInteger n a)
            (a, b) -> return (LispRational n (a % b))

    (LispBool n b) ->
        return (LispBool n b)

    (LispString n s) ->
        return (LispString n s)

    (LispCharacter n c) ->
        return (LispCharacter n c)

    (LispPair n p) ->
        return (LispPair n p)

    (LispList n []) ->
        return (LispBool n False)

    (LispList _ [LispSymbol n label]) -> do
        sLabels <- syntaxLabels
        if label `elem` sLabels then do
            f <- syntaxReference (LispSymbol n label)
            f n []
        else do
            f <- procedureReference (LispSymbol n label)
            f n []
    
    (LispList _ (LispSymbol n label : args)) -> do
        sLabels <- syntaxLabels
        if label `elem` sLabels then do
            f <- syntaxReference (LispSymbol n label)
            f n args
        else do
            f <- procedureReference (LispSymbol n label)
            args' <- mapM eval args
            f n args'

    (LispList _ xs) ->
        throwE (IllegalFunctionCall (index (head xs)))
