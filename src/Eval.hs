{-# LANGUAGE LambdaCase #-}

module Eval (eval) where

import LispData
import LispEnv
import LispError (RuntimeError(..))

import Control.Monad.Trans.Except (throwE)
import Data.Functor ((<&>))

eval :: LispData -> Eval
eval = \case
    (LispSymbol n s) ->
        variableReference (LispSymbol n s)

    (LispInteger n z) ->
        return (LispInteger n z)

    (LispReal n r) ->
        return (LispReal n r)

    (LispRational n a b) ->
        return (LispRational n a b)

    (LispBool n b) ->
        return (LispBool n b)

    (LispString n s) ->
        return (LispString n s)

    (LispCharacter n c) ->
        return (LispCharacter n c)

    (LispList n []) ->
        return (LispBool n False)

    (LispList _ [LispSymbol n "if"]) ->
        throwE (SyntaxError n "if")

    (LispList _ (LispSymbol n "if" : args)) ->
        case args of
            [test, body1, body2] -> do
                cond <- eval test <&> treatAsLispBool
                if cond then eval body1
                        else eval body2

            [test, body] -> do
                cond <- eval test <&> treatAsLispBool
                if cond then eval body
                        else return (LispBool n False)

            _ ->
                throwE (SyntaxError n "if")
    
    (LispList _ [LispSymbol n label]) -> do
        f <- functionReference (LispSymbol n label)
        f n []
    
    (LispList _ (LispSymbol n label : args)) -> do
        f     <- functionReference (LispSymbol n label)
        args' <- mapM eval args
        f n args'

    (LispList _ xs) ->
        throwE (IllegalFunctionCall (index (head xs)))
