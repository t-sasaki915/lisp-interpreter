{-# LANGUAGE LambdaCase #-}

module Eval (eval) where

import LispError (RuntimeError(..))
import LispOperation
import LispSystem
import Util ((~>))

import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.State.Strict (get)
import Data.Functor ((<&>))
import Data.Ratio ((%), numerator, denominator)

eval :: LispData -> Execution LispData
eval = \case
    (LispSymbol label) -> do
        (globe, lexi) <- lift get <&> transformEnv
        case lookup label lexi of
            Just (LispVariable d) ->
                return d

            Just LispVariableBind ->
                throwE (UninitialisedVariable label)

            _ ->
                case lookup label globe of
                    Just (LispVariable d) ->
                        return d

                    Just LispVariableBind ->
                        throwE (UninitialisedVariable label)

                    _ ->
                        throwE (UndefinedVariable label)

    (LispQuote d) ->
        return d

    (LispInteger z) ->
        return (LispInteger z)

    (LispReal r) ->
        return (LispReal r)

    (LispRational r) ->
        case (numerator r, denominator r) of
            (a, 1) -> return (LispInteger a)
            (a, b) -> return (LispRational (a % b))

    (LispBool b) ->
        return (LispBool b)

    (LispString s) ->
        return (LispString s)

    (LispCharacter c) ->
        return (LispCharacter c)

    (LispPair p) ->
        return (LispPair p)

    (LispClosure b p) ->
        return (LispClosure b p)

    (LispList []) ->
        return (LispBool False)

    (LispList lst) ->
        case head lst of
            (LispSymbol label) -> do
                (globe, _) <- lift get <&> transformEnv
                case lookup label globe of
                    Just (LispSyntax f) ->
                        f (drop 1 lst)

                    Just (LispFunction f) -> do
                        args <- mapM eval (drop 1 lst)
                        f args

                    Just (LispVariable (LispClosure binds progs)) -> do
                        args    <- mapM eval (drop 1 lst)
                        newLexi <- attribute label binds args
                        _       <- lexicalScope newLexi
                        values  <- mapM eval progs
                        _       <- finaliseLexicalScope
                        return (last values)

                    Just (LispVariable _) ->
                        throwE IllegalFunctionCall
                       
                    Just LispVariableBind ->
                        throwE (UninitialisedVariable label)

                    Nothing ->
                        throwE (UndefinedFunction label)

            _ ->
                throwE IllegalFunctionCall

attribute :: String -> [(String, LispEnvData)] ->
             [LispData] -> Execution [(String, LispEnvData)]
attribute label lexi args = do
    (lexi', refIndex) <- foldM
        (\(lst, refIndex) -> \case
            (_, LispVariableBind) | refIndex >= length args ->
                throwE (TooFewArguments label refIndex)

            (lb, LispVariableBind) ->
                return (lst ++ [lb ~> LispVariable (args !! refIndex)], refIndex + 1)

            (lb, d) ->
                return (lst ++ [lb ~> d], refIndex)
        )
        ([], 0)
        lexi

    if refIndex == length args
        then return lexi'
        else throwE (TooManyArguments label refIndex)
