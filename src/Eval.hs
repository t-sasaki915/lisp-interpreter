{-# LANGUAGE LambdaCase #-}

module Eval (eval) where

import LispError (RuntimeError(..))
import LispOperation
import LispSystem
import Util ((~>))

import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.State.Strict (get, put)
import Data.Functor ((<&>))
import Data.List (find)
import Data.Ratio ((%), numerator, denominator)

eval :: LispData -> Execution LispData
eval = \case
    (LispSymbol n s) -> do
        (globe, lexi) <- lift get <&> transformEnv
        case find (\(l, _) -> l == s) lexi of
            Just (_, LispVariable d) ->
                return d

            Just (_, LispVariableBind) ->
                throwE (UninitialisedVariable n s)

            _ ->
                case find (\(l, _) -> l == s) globe of
                    Just (_, LispVariable d) ->
                        return d

                    Just (_, LispVariableBind) ->
                        throwE (UninitialisedVariable n s)

                    _ ->
                        throwE (UndefinedVariable n s)

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

    (LispClosure n b p) ->
        return (LispClosure n b p)

    (LispList n []) ->
        return (LispBool n False)

    (LispList _ lst) ->
        case head lst of
            (LispSymbol n' label) -> do
                (globe, _) <- lift get <&> transformEnv
                case find (\(l, _) -> l == label) globe of
                    Just (_, LispSyntax f) ->
                        f n' (drop 1 lst)

                    Just (_, LispFunction f) -> do
                        args <- mapM eval (drop 1 lst)
                        f n' args

                    Just (_, LispVariable (LispClosure _ binds prog)) -> do
                        args       <- mapM eval (drop 1 lst)
                        newLexi    <- attribute n' label binds args
                        _          <- lift $ put (LispEnv globe newLexi)
                        value      <- eval prog
                        _          <- restoreEnv
                        return value

                    Just (_, LispVariable _) ->
                        throwE (IllegalFunctionCall ((fst . indexAndType) (head lst)))
                       
                    Just (_, LispVariableBind) ->
                        throwE (UninitialisedVariable n' label)

                    Nothing ->
                        throwE (UndefinedFunction n' label)

            _ ->
                throwE (IllegalFunctionCall ((fst . indexAndType) (head lst)))

attribute :: Int -> String -> [(String, LispEnvData)] ->
             [LispData] -> Execution [(String, LispEnvData)]
attribute ind label lexi args = do
    (lexi', refIndex) <- foldM
        (\(lst, refIndex) -> \case
            (_, LispVariableBind) | refIndex >= length args ->
                throwE (TooFewArguments ind label refIndex)

            (lb, LispVariableBind) ->
                return (lst ++ [lb ~> LispVariable (args !! refIndex)], refIndex + 1)

            (lb, d) ->
                return (lst ++ [lb ~> d], refIndex)
        )
        ([], 0)
        lexi

    if refIndex == length args
        then return lexi'
        else throwE (TooManyArguments ind label refIndex)

restoreEnv :: Execution ()
restoreEnv = do
    (globe, _) <- lift get <&> transformEnv
    _          <- lift $ put (LispEnv globe [])
    return ()
