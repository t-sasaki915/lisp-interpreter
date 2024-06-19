{-# LANGUAGE LambdaCase #-}

module LispInterpreter (translateSyntax, evaluateLisp) where

import LispData
import LispError (LispError(..))
import Syntax (Syntax(..))

import Control.Lens (set)
import Control.Monad (foldM)
import Control.Monad.Trans.Except (Except, ExceptT, throwE)
import Data.Maybe (fromMaybe)
import Data.List (find)

translateSyntax :: LispState -> [Syntax] -> Except LispError (LispState, [LispData])
translateSyntax state =
    foldM
        (\(st, dt) syntax ->
            case syntax of
                (InstantList n lst) ->
                    translateSyntax st lst >>=
                        (\(st', lst') ->
                            return
                                (st', dt ++ [LispList n lst']))

                (LazyList n lst) ->
                    translateSyntax st lst >>=
                        (\(st', lst') ->
                            return
                                (st', dt ++ [LispLazyList n lst']))

                (NumberRef n x) ->
                    return (st, dt ++ [LispNumber n x])

                (StringRef n s) ->
                    return (st, dt ++ [LispString n s])

                (BoolRef n b) ->
                    return (st, dt ++ [LispBool n b])

                (IdentifierRef n i) ->
                    return (st, dt ++ [LispIdentifier n i])       
        )
        (state, [])


evaluateLisp :: LispState -> [LispData] -> ExceptT LispError IO (LispState, [LispData])
evaluateLisp state =
    foldM
        (\(st, dt) dat -> do
            case dat of
                (LispList n []) ->
                    return (st, dt ++ [LispList n []])

                (LispList n [LispFunction _ _ prog]) -> do
                    evalList dt prog n st []

                (LispList n (LispFunction _ _ prog : args)) ->
                    evalList dt prog n st args

                (LispList n' [LispIdentifier n lb]) ->
                    case find (funcFilt lb) (_functions st) of
                        Just (LispFunction _ _ prog) ->
                            evalList dt prog n' st []

                        _ ->
                            throwE $ UndefinedFunction n lb

                (LispList n' (LispIdentifier n lb : args)) ->
                    case find (funcFilt lb) (_functions st) of
                        Just (LispFunction _ _ prog) ->
                            evalList dt prog n' st args

                        _ ->
                            throwE $ UndefinedFunction n lb

                (LispList _ [d]) ->
                    throwE $ UndefinedFunction (lispDataIndex d) (show d)

                (LispList _ (d : _)) ->
                    throwE $ UndefinedFunction (lispDataIndex d) (show d)

                (LispLazyList n lst) ->
                    return (st, dt ++ [LispLazyList n lst])

                (LispNumber n x) ->
                    return (st, dt ++ [LispNumber n x])

                (LispString n s) ->
                    return (st, dt ++ [LispString n s])

                (LispBool n b) ->
                    return (st, dt ++ [LispBool n b])

                (LispVariable _ _ v) ->
                    return (st, dt ++ [v])

                (LispFunction n l p) ->
                    return (st, dt ++ [LispFunction n l p])

                (LispIdentifier n lb) ->
                    let
                        search filt store = find (filt lb) (store st) >>=
                            (\case
                                (LispVariable _ _ v) -> Just $ return (st, dt ++ [v])
                                x -> Just $ return (st, dt ++ [x])
                            )
                    in
                    fromMaybe
                        ( fromMaybe
                            ( fromMaybe
                                (throwE $ UndefinedIdentifier n lb)
                                (search funcFilt _functions)
                            )
                            (search varFilt _variables)
                        )
                        (search varFilt _localVariables)
        )
        (state, [])
    where
        evalList dt prog ind st args = do
            (st', d) <- prog ind st args
            return (set localVariables plvars st', dt ++ [d])
            where plvars = _localVariables st
