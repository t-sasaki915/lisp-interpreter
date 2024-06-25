{-# LANGUAGE LambdaCase #-}

module Eval (eval, lexicalScope) where

import LispError (RuntimeError(..))
import LispOperation
import LispSystem

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.State.Strict (get)
import Data.List (find)
import Data.Ratio ((%), numerator, denominator)

eval :: LispData -> Execution LispData
eval = \case
    (LispSymbol n s) -> do
        env <- lift get
        case find (\(l, _) -> l == s) env of
            Just (_, LispVariable _ d) ->
                return d

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

    (LispList n []) ->
        return (LispBool n False)

    (LispList n lst) ->
        case head lst of
            (LispSymbol n' label) -> do
                env <- lift get
                case find (\(l, _) -> l == label) env of
                    Just (_, LispSyntax _ f) ->
                        f n' (drop 1 lst)
                    
                    Just (_, LispFunction _ f) -> do
                        args <- mapM eval (drop 1 lst)
                        f n' args

                    Just (_, LispVariable _ f) ->
                        eval (LispList n (f : drop 1 lst))

                    _ ->
                        throwE (UndefinedFunction n' label)

            (LispSyntax n' f) ->
                f n' (drop 1 lst)

            (LispFunction n' f) -> do
                args <- mapM eval (drop 1 lst)
                f n' args
            
            (LispList n' lst') -> do
                res <- eval (LispList n' lst')
                eval (LispList n (res : drop 1 lst))

            _ ->
                throwE (IllegalFunctionCall ((fst . indexAndType) (head lst)))
    
    _ ->
        throwE (IllegalBehaviour 0)

lexicalScope :: [String] -> [LispData] -> [LispData] -> Execution LispData
lexicalScope binds tempData progs = do
    initialEnv <- lift get
    let initialBinds =
            zipWith
                (\l _ -> case find (\(l', _) -> l == l') initialEnv of
                    Just (_, d) -> (l, Just d)
                    Nothing     -> (l, Nothing)
                )
                binds
                tempData

    mapM_ (uncurry putEnvData) (zip binds tempData)
    res <- mapM eval progs

    mapM_
        (\(l, md) -> case md of
            Just d  -> putEnvData l d
            Nothing -> unbindEnvData l
        )
        initialBinds
    return (last res)
