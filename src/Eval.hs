{-# LANGUAGE LambdaCase #-}

module Eval (eval, evalClosure) where

import LispError (RuntimeError(..))
import LispOperation
import LispSystem
import Util ((~>))

import Control.Monad.Trans.Except (throwE)
import Data.Functor ((<&>))
import Data.Ratio ((%), numerator, denominator)

eval :: LispData -> Execution LispData
eval = \case
    (LispSymbol label) -> do
        maybeLexicalVar <- lookupLexicalVariable label
        case maybeLexicalVar of
            Just (Just d) -> return d
            Just Nothing  -> throwE (UninitialisedVariable label) 
            Nothing -> do
                maybeGlobalVar <- lookupGlobalVariable label
                case maybeGlobalVar of
                    Just (Just d) -> return d
                    Just Nothing  -> throwE (UninitialisedVariable label)
                    _             -> throwE (UndefinedVariable label)

    (LispQuote d) ->
        return d

    (LispRational r) ->
        case (numerator r, denominator r) of
            (a, 1) -> return (LispInteger a)
            (a, b) -> return (LispRational (a % b))

    (LispList []) ->
        return (LispBool False)

    (LispList lst) -> case head lst of
        (LispSymbol label) -> do
            maybeFunc <- lookupFunction label
            case maybeFunc of
                Just (LispFunction f) -> do
                    args <- mapM eval (drop 1 lst)
                    f args

                Just (LispSyntax f) ->
                    f (drop 1 lst)

                Just (LispUserFunction (LispClosure binds labels progs)) ->
                    evalClosure label (LispClosure binds labels progs) (drop 1 lst)

                _ ->
                    throwE (UndefinedFunction label)
        _ ->
            throwE IllegalFunctionCall

    d ->
        return d

evalClosure :: String -> LispData -> [LispData] -> Execution LispData
evalClosure label (LispClosure binds labels progs) args = do
    binds' <- attribute
    _      <- lexicalScope binds'
    value  <- mapM eval progs <&> last
    _      <- finaliseLexicalScope
    return value
    where
        attribute
            | length args > length labels = throwE (TooManyArguments label (length labels))
            | length args < length labels = throwE (TooFewArguments label (length labels))
            | otherwise = do
                args' <- mapM eval args
                return $ fst $
                    foldl (\(lst, ref) lb -> (lst ++ [lb ~> Just (args' !! ref)], ref + 1))
                        (binds, 0) labels
evalClosure _ d _ = throwE (IncompatibleType (dataType d) "CLOSURE")
