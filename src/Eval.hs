module Eval (eval) where

import LispData
import LispEnv
import LispError (RuntimeError(..))

import Control.Monad.Trans.Except (throwE)
import Data.Functor ((<&>))

eval :: LispData -> Eval
eval ld = case ld of
    (LispSymbol _ _) ->
        variableReference ld

    _ | isAtom ld ->
        return ld

    (LispList _ []) ->
        return ld

    (LispList _ [LispSymbol n "if"]) ->
        throwE (SyntaxError n "if")

    (LispList _ (LispSymbol n "if" : args)) ->
        case args of
            [test, body1, body2] -> do
                cond <- eval test <&> treatAsLispBool
                if cond
                    then eval body1
                    else eval body2

            [test, body] -> do
                cond <- eval test <&> treatAsLispBool
                if cond
                    then eval body
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

    _ ->
        throwE (IllegalBehaviour (-1))
