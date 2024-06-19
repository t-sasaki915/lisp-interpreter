{-# LANGUAGE LambdaCase #-}

module Tokeniser (TokeniserError(..), tokenise) where

import ErrorTrace (Tracable(..))
import Token (Token(..))

import Control.Monad (foldM)
import Control.Monad.Trans.Except (Except, throwE)
import Text.Regex.Posix ((=~))

data TokeniserError = InvalidNumberFormat Int String
                    | UnexpectedEOF Int
                    deriving (Show, Eq)

instance Tracable TokeniserError where
    place (InvalidNumberFormat a _) = a
    place (UnexpectedEOF a)         = a

    title (InvalidNumberFormat {})  = "Invalid number format"
    title (UnexpectedEOF {})        = "Unexpected end of file"

    cause (InvalidNumberFormat _ a) = a
    cause (UnexpectedEOF _)         = ""

data Status = ExpectingCharacter
            | BufferingIdentifierOrNumber String
            | BufferingStringLiteral String
            | IgnoringCharacter

tokenise :: String -> Except TokeniserError [Token]
tokenise src = do
    (tokens, status) <- tokenise' src
    case status of
        ExpectingCharacter -> return tokens
        _ -> throwE $ UnexpectedEOF 0

tokenise' :: String -> Except TokeniserError ([Token], Status)
tokenise' src =
    foldM
        (\case
            (tokens, ExpectingCharacter) -> \case
                (n, '(') ->
                    return (tokens ++ [OpenParentheses n], ExpectingCharacter)

                (n, ')') ->
                    return (tokens ++ [CloseParentheses n], ExpectingCharacter)

                (n, '\'') ->
                    return (tokens ++ [SingleQuote n], ExpectingCharacter)

                (_, '"') ->
                    return (tokens, BufferingStringLiteral "")

                (_, ';') ->
                    return (tokens, IgnoringCharacter)

                (_, c) | c `elem` [' ', '\t', '\n'] ->
                    return (tokens, ExpectingCharacter)

                (_, c) ->
                    return (tokens, BufferingIdentifierOrNumber [c])
            
            (tokens, BufferingStringLiteral buffer) -> \case
                (n, '"') ->
                    return (tokens ++ [StringLiteral n buffer], ExpectingCharacter)

                (_, c) ->
                    return (tokens, BufferingStringLiteral $ buffer ++ [c])

            (tokens, BufferingIdentifierOrNumber buffer) -> \case
                (n, '(') ->
                    determineIdentifier (n - 1) buffer >>=
                        (\a ->
                            return ( tokens ++ [a, OpenParentheses n]
                                   , ExpectingCharacter
                                   )
                        )

                (n, ')') ->
                    determineIdentifier (n - 1) buffer >>=
                        (\a ->
                            return ( tokens ++ [a, CloseParentheses n]
                                   , ExpectingCharacter
                                   )
                        )
                
                (n, '\'') ->
                    determineIdentifier (n - 1) buffer >>=
                        (\a ->
                            return ( tokens ++ [a, SingleQuote n]
                                   , ExpectingCharacter
                                   )
                        )

                (n, c) | c `elem` [' ', '\t', '\n'] ->
                    determineIdentifier (n - 1) buffer >>=
                        (\a ->
                            return ( tokens ++ [a]
                                   , ExpectingCharacter
                                   )
                        )

                (n, '"') ->
                    determineIdentifier (n - 1) buffer >>=
                        (\a ->
                            return ( tokens ++ [a]
                                   , BufferingStringLiteral ""
                                   )
                        )

                (n, ';') ->
                    determineIdentifier (n - 1) buffer >>=
                        (\a ->
                            return ( tokens ++ [a]
                                   , IgnoringCharacter
                                   )
                        )

                (_, c) ->
                    return (tokens, BufferingIdentifierOrNumber $ buffer ++ [c])

            (tokens, IgnoringCharacter) -> \case
                (_, '\n') ->
                    return (tokens, ExpectingCharacter)

                _ ->
                    return (tokens, IgnoringCharacter)

        )
        ([], ExpectingCharacter)
        (zip [0..] src)
    where
        determineIdentifier :: Int -> String -> Except TokeniserError Token
        determineIdentifier n str | head str `elem` numbers =
            case str of
                "1+" -> return $ Identifier n "1+"
                "1-" -> return $ Identifier n "1-"
                _ | str == str =~ "([0-9]{1,}.)?[0-9]{1,}" -> return $ Number n str
                _ -> throwE $ InvalidNumberFormat n str
            where numbers = map (head . show) ([0..9] :: [Int])
        
        determineIdentifier n str =
            case str of
                "t"   -> return $ BoolLiteral n "t"
                "nil" -> return $ BoolLiteral n "nil"
                _     -> return $ Identifier n str
