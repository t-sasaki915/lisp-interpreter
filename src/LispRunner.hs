{-# LANGUAGE LambdaCase #-}

module LispRunner (runLisp) where

import Lisp
import SyntaxAnalyser (LispSyntax(..))
import Token (Token(..), tokenLetter)

import Data.List (find)

runLisp :: [LispSyntax] -> IO (Either LispError ())
runLisp program =
    case mapM (parseSyntax initState) program of
        Right xs -> do
            print xs
            return $ Right ()

        Left err ->
            return $ Left err
    where
        initState = LispState lispPredefinedFunctions []

parseSyntax :: LispState -> LispSyntax -> Either LispError LispData
parseSyntax state =
    \case
        (RawList lst) ->
            mapM (parseSyntax state) lst >>= Right . LispRawList

        (ExecList lst) ->
            mapM (parseSyntax state) lst >>= Right . LispList

        (NumValue (Number _ n)) ->
            Right $ LispNumber (read n)

        (StrValue (StringLiteral _ s)) ->
            Right $ LispString s

        (BoolValue (BoolLiteral _ "t")) ->
            Right $ LispBool True

        (BoolValue (BoolLiteral _ "nil")) ->
            Right $ LispBool False

        (VarValue (Identifier _ var)) ->
            case find (functionMatch var) (_functions state) of
                Just f ->
                    Right f

                Nothing ->
                    case find (variableMatch var) (_variables state) of
                        Just f ->
                            Right f

                        Nothing ->
                            Left $ UndefinedIdentifier var

        (NumValue t) ->
            Left $ InvalidToken (tokenLetter t)

        (StrValue t) ->
            Left $ InvalidToken (tokenLetter t)

        (BoolValue t) ->
            Left $ InvalidToken (tokenLetter t)

        (VarValue t) ->
            Left $ InvalidToken (tokenLetter t)
    where
        functionMatch n = \case (LispFunction name _) -> n == name
                                _                     -> False
        variableMatch n = \case (LispVariable name _)   -> n == name
                                (LispVariableBind name) -> n == name
                                _                       -> False
