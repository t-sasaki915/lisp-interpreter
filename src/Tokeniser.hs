module Tokeniser (TokeniserError(..), tokenise) where

import ErrorTrace (Tracable(..))
import Token (Token(..))

import Text.Regex.Posix

data TokeniserError = InvalidNumberFormat String Int String
                    | UnexpectedEOF String Int
                    deriving (Show, Eq)

instance Tracable TokeniserError where
    place (InvalidNumberFormat a b _) = (a, b)
    place (UnexpectedEOF a b)         = (a, b)

    title (InvalidNumberFormat {})    = "Invalid number format"
    title (UnexpectedEOF {})          = "Unexpected end of file"

    cause (InvalidNumberFormat _ _ a) = a
    cause (UnexpectedEOF {})          = ""

data Status = ExpectingCharacter
            | BufferingIdentifierOrNumber String
            | BufferingStringLiteral String
            | IgnoringCharacter

type Result = Either TokeniserError [Token]

tokenise :: String -> Result
tokenise src =
    case foldl (tokenise' src) (Right [], ExpectingCharacter) (zip [0..] src) of
        (Right tokens, ExpectingCharacter) ->
            Right tokens

        (Right _, _) ->
            Left $ UnexpectedEOF src 0

        (Left err, _) ->
            Left err

tokenise' :: String -> (Result, Status) -> (Int, Char) -> (Result, Status)
tokenise' src state character =
    case state of
        (Left _, _) -> state
        (Right tokens, ExpectingCharacter) ->
            case character of
                (n, '(') ->
                    (Right $ tokens ++ [OpenParentheses n], ExpectingCharacter)

                (n, ')') ->
                    (Right $ tokens ++ [CloseParentheses n], ExpectingCharacter)

                (n, '\'') ->
                    (Right $ tokens ++ [SingleQuote n], ExpectingCharacter)

                (_, '"') ->
                    (Right tokens, BufferingStringLiteral "")

                (_, ';') ->
                    (Right tokens, IgnoringCharacter)

                (_, c) | c `elem` [' ', '\t', '\n'] ->
                    (Right tokens, ExpectingCharacter)

                (_, c) ->
                    (Right tokens, BufferingIdentifierOrNumber [c])

        (Right tokens, BufferingStringLiteral buffer) ->
            case character of
                (n, '"') ->
                    (Right $ tokens ++ [StringLiteral n buffer], ExpectingCharacter)

                (_, c) ->
                    (Right tokens, BufferingStringLiteral $ buffer ++ [c])

        (Right tokens, BufferingIdentifierOrNumber buffer) ->
            case character of
                (n, '(') ->
                    case considerIdentifierOrNumber (n - 1) buffer of
                        Right t ->
                            (Right $ tokens ++ [t, OpenParentheses n], ExpectingCharacter)

                        Left e ->
                            (Left e, ExpectingCharacter)
                
                (n, ')') ->
                    case considerIdentifierOrNumber (n - 1) buffer of
                        Right t ->
                            (Right $
                                tokens ++ [t, CloseParentheses n], ExpectingCharacter)

                        Left e ->
                            (Left e, ExpectingCharacter)

                (n, '\'') ->
                    case considerIdentifierOrNumber (n - 1) buffer of
                        Right t ->
                            (Right $ tokens ++ [t, SingleQuote n], ExpectingCharacter)

                        Left e ->
                            (Left e, ExpectingCharacter)

                (n, c) | c `elem` [' ', '\t', '\n'] ->
                    case considerIdentifierOrNumber (n - 1) buffer of
                        Right t ->
                            (Right $ tokens ++ [t], ExpectingCharacter)

                        Left e ->
                            (Left e, ExpectingCharacter)

                (n, ';') ->
                    case considerIdentifierOrNumber (n - 1) buffer of
                        Right t ->
                            (Right $ tokens ++ [t], IgnoringCharacter)

                        Left e ->
                            (Left e, ExpectingCharacter)

                (_, c) ->
                    (Right tokens, BufferingIdentifierOrNumber $ buffer ++ [c])

        (Right tokens, IgnoringCharacter) ->
            case character of
                (_, '\n') ->
                    (Right tokens, ExpectingCharacter)

                _ ->
                    (Right tokens, IgnoringCharacter)
    where
        considerIdentifierOrNumber :: Int -> String -> Either TokeniserError Token
        considerIdentifierOrNumber n (x : xs) | x `elem` numbers =
            case x : xs of
                str | str == str =~ "([0-9]{1,}.)?[0-9]{1,}" -> Right $ Number n str
                _ -> Left $ InvalidNumberFormat src n (x : xs)
            where numbers = map (head . show) ([0..9] :: [Int])

        considerIdentifierOrNumber n buffer = Right $ Identifier n buffer
