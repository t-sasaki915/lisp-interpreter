{-# LANGUAGE LambdaCase #-}

module LispRunner (runLisp) where

import Lisp
import SyntaxAnalyser (LispSyntax(..))
import Token (Token(..), tokenLetter)

import Data.List (find)
import Control.Monad (foldM)

runLisp :: [LispSyntax] -> IO (Either LispError ())
runLisp program = do
    result <- parseSyntaxes initState program
    case result of
        Right (_, lispData) -> do
            print $ last lispData
            return $ Right ()

        Left err ->
            return $ Left err
    where
        initState = LispState lispPredefinedFunctions []

parseSyntaxes :: LispState -> [LispSyntax]-> IO (Either LispError (LispState, [LispData]))
parseSyntaxes state =
    foldM
        (\state' syn ->
            case state' of
                (Left _) -> return state'
                (Right (st, lst)) -> do
                    result <- parseSyntax st syn
                    case result of
                        Right (newState, value) ->
                            return $ Right (newState, lst ++ [value])

                        Left err ->
                            return $ Left err
        )
        (Right (state, []))

parseSyntax :: LispState -> LispSyntax -> IO (Either LispError (LispState, LispData))
parseSyntax state =
    \case
        (RawList lst) -> do
            result <- parseSyntaxes state lst
            case result of
                Right (newState, lst') ->
                    return $ Right (newState, LispList lst')

                Left err ->
                    return $ Left err

        (ExecList []) ->
            return $ Left EmptyExecList

        (ExecList [VarValue (Identifier _ var)]) ->
            case find (functionMatch var) (_functions state) of
                Just (LispFunction _ prog) -> do
                    result <- prog [] state
                    case result of
                        Right (newState, value) ->
                            return $ Right (newState, value)

                        Left err ->
                            return $ Left err

                _ ->
                    return $ Left (UndefinedIdentifier var)

        (ExecList (VarValue (Identifier _ var) : args)) ->
            case find (functionMatch var) (_functions state) of
                Just (LispFunction _ prog) -> do
                    result <- parseSyntaxes state args
                    case result of
                        Right (newState, args') -> do
                            result2 <- prog args' newState
                            case result2 of
                                Right (newState', value) ->
                                    return $ Right (newState', value)

                                Left err ->
                                    return $ Left err
                        Left err ->
                            return $ Left err

                _ ->
                    return $ Left (UndefinedIdentifier var)

        (NumValue (Number _ n)) ->
            return $ Right (state, LispNumber (read n))

        (StrValue (StringLiteral _ s)) ->
            return $ Right (state, LispString s)

        (BoolValue (BoolLiteral _ "t")) ->
            return $ Right (state, LispBool True)

        (BoolValue (BoolLiteral _ "nil")) ->
            return $ Right (state, LispBool False)

        (VarValue (Identifier _ var)) ->
            case find (functionMatch var) (_functions state) of
                Just f ->
                    return $ Right (state, f)

                Nothing ->
                    case find (variableMatch var) (_variables state) of
                        Just (LispVariable _ v) ->
                            return $ Right (state, v)

                        Just f ->
                            return $ Right (state, f)

                        Nothing ->
                            return $ Right (state, LispVariableBind var)

        (NumValue t) ->
            return $ Left (InvalidToken (tokenLetter t))

        (StrValue t) ->
            return $ Left (InvalidToken (tokenLetter t))

        (BoolValue t) ->
            return $ Left (InvalidToken (tokenLetter t))

        (VarValue t) ->
            return $ Left (InvalidToken (tokenLetter t))

        (ExecList _) ->
            return $ Left InvalidProgramFormat
    where
        functionMatch n = \case (LispFunction name _) -> n == name
                                _                     -> False
        variableMatch n = \case (LispVariable name _)   -> n == name
                                (LispVariableBind name) -> n == name
                                _                       -> False
