{-# LANGUAGE LambdaCase #-}

module LispInterpreter (translate, evaluate) where

import LispData
import LispError (LispError(..))
import Syntax (Syntax(..))

import Control.Lens (set)
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)
import Data.List (find)

translate :: LispState -> [Syntax] -> Either LispError (LispState, [LispData])
translate state =
    foldl
        (\acc syntax ->
            case acc of
                (Left _) -> acc
                (Right (st, dt)) ->
                    case syntax of
                        (InstantList n lst) ->
                            translate st lst >>=
                                (\(st', lst') ->
                                    Right (st', dt ++ [LispList n lst']))

                        (LazyList n lst) ->
                            translate st lst >>=
                                (\(st', lst') ->
                                    Right (st', dt ++ [LispLazyList n lst']))

                        (NumberRef n x) ->
                            Right (st, dt ++ [LispNumber n x])

                        (StringRef n s) ->
                            Right (st, dt ++ [LispString n s])

                        (BoolRef n b) ->
                            Right (st, dt ++ [LispBool n b])

                        (IdentifierRef n i) ->
                            Right (st, dt ++ [LispIdentifier n i])       
        )
        (Right (state, []))


evaluate :: LispState -> [LispData] -> IO (Either LispError (LispState, [LispData]))
evaluate state =
    foldM
        (\acc dat ->
            case acc of
                (Left _) -> return acc
                (Right (st, dt)) ->
                    case dat of
                        (LispList n []) ->
                            return $
                                Right (st, dt ++ [LispList n []])

                        (LispList n [LispFunction _ _ prog]) ->
                            evalList dt prog n st []

                        (LispList n (LispFunction _ _ prog : args)) ->
                            evalList dt prog n st args

                        (LispList n' [LispIdentifier n lb]) ->
                            case find (funcFilt lb) (_functions st) of
                                Just (LispFunction _ _ prog) ->
                                    evalList dt prog n' st []

                                _ ->
                                    return $ Left (UndefinedFunction n lb)

                        (LispList n' (LispIdentifier n lb : args)) ->
                            case find (funcFilt lb) (_functions st) of
                                Just (LispFunction _ _ prog) ->
                                    evalList dt prog n' st args

                                _ ->
                                    return $ Left (UndefinedFunction n lb)

                        (LispList _ [d]) ->
                            return $
                                Left (UndefinedFunction (lispDataIndex d) (show d))

                        (LispList _ (d : _)) ->
                            return $
                                Left (UndefinedFunction (lispDataIndex d) (show d))

                        (LispLazyList n lst) ->
                            return $
                                Right (st, dt ++ [LispLazyList n lst])

                        (LispNumber n x) ->
                            return $
                                Right (st, dt ++ [LispNumber n x])

                        (LispString n s) ->
                            return $
                                Right (st, dt ++ [LispString n s])

                        (LispBool n b) ->
                            return $
                                Right (st, dt ++ [LispBool n b])

                        (LispVariable _ _ v) ->
                            return $
                                Right (st, dt ++ [v])

                        (LispFunction n l p) ->
                            return $
                                Right (st, dt ++ [LispFunction n l p])

                        (LispVariableBind n l) ->
                            return $
                                Left (UninitialisedVariableAccess n l)

                        (LispIdentifier n lb) ->
                            let
                                search filt store = find (filt lb) (store st) >>=
                                    (\case
                                        (LispVariable _ _ v) -> Just $ Right (st, v)
                                        x -> Just $ Right (st, x)
                                    )
                                res1 =
                                    fromMaybe
                                        ( fromMaybe
                                            ( fromMaybe
                                                (Left $ UndefinedIdentifier n lb)
                                                (search varFilt _variables)
                                            )
                                            (search varFilt _localVariables)
                                        )
                                        (search funcFilt _functions)
                            in
                            case res1 of
                                Right (st', d) ->
                                    return $
                                        Right (st', dt ++ [d])

                                Left undefErr ->
                                    case find (varBindFilt lb) (_localVariables st) of
                                        Just _ ->
                                            return $
                                                Left (UninitialisedVariableAccess n lb)

                                        Nothing ->
                                            case find (varBindFilt lb) (_variables st) of
                                                Just _ ->
                                                    return $
                                                        Left 
                                                            (UninitialisedVariableAccess n lb)

                                                Nothing ->
                                                    return $ Left undefErr
        )
        (Right (state, []))
    where
        evalList dt prog ind st args =
            let plvars = _localVariables st in
            do  res <- prog ind st args
                case res of
                    Right (st', d) ->
                        return $
                            Right
                                ( set localVariables plvars st'
                                , dt ++ [d]
                                )
                    
                    Left err ->
                        return $ Left err
