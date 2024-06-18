module SyntaxAnalyser
    ( SyntaxAnalyserError(..)
    , syntaxAnalyse
    ) where

import ErrorTrace (Tracable(..))
import Syntax (Syntax(..))
import Token (Token(..), tokenIndex, tokenLetter)

data SyntaxAnalyserError = UnexpectedToken String Int String String
                         | UnexpectedEOF String Int
                         deriving (Eq, Show)

instance Tracable SyntaxAnalyserError where
    place (UnexpectedToken a b _ _) = (a, b)
    place (UnexpectedEOF a b)       = (a, b)

    title (UnexpectedToken {})      = "Unexpected token"
    title (UnexpectedEOF {})        = "Unexpected end of file"

    cause (UnexpectedToken _ _ a b) =
        "The interpreter was expecting " ++ b ++ ", but what found was " ++ a ++ "."
    cause (UnexpectedEOF _ _)       = ""

data State = ExpectingOpenParenthesesOrSingleQuote
           | ExpectingOpenParentheses 
           | MakingInstantList Int [Syntax]
           | MakingLazyList Int [Syntax]

syntaxAnalyse :: String -> [Token] -> Either SyntaxAnalyserError [Syntax]
syntaxAnalyse src tokens = topLevel ExpectingOpenParenthesesOrSingleQuote 0 []
    where
        topLevel :: State -> Int -> [Syntax] ->
                    Either SyntaxAnalyserError [Syntax]
        topLevel state index determined | index >= length tokens =
            case state of
                ExpectingOpenParenthesesOrSingleQuote ->
                    Right determined

                _ ->
                    Left $ UnexpectedEOF src 0
        
        topLevel ExpectingOpenParenthesesOrSingleQuote index determined =
            case tokens !! index of
                (OpenParentheses _) ->
                    topLevel (MakingInstantList 0 []) index determined
            
                (SingleQuote _) ->
                    topLevel ExpectingOpenParentheses (index + 1) determined

                t ->
                    unexpectedToken t "'(' or '''"
        
        topLevel ExpectingOpenParentheses index determined =
            case tokens !! index of
                (OpenParentheses _) ->
                    topLevel (MakingLazyList 0 []) (index - 1) determined

                t ->
                    unexpectedToken t "'('"
        
        topLevel (MakingInstantList _ _) index determined =
            secondLevel ExpectingOpenParenthesesOrSingleQuote index >>=
                (\(newIndex, program) ->
                    topLevel
                        ExpectingOpenParenthesesOrSingleQuote
                            (newIndex + 1)
                                (determined ++ [program])
                )

        topLevel (MakingLazyList _ _) index determined =
            secondLevel ExpectingOpenParenthesesOrSingleQuote index >>=
                (\(newIndex, program) ->
                    topLevel
                        ExpectingOpenParenthesesOrSingleQuote
                            (newIndex + 1)
                                (determined ++ [program])
                )

        secondLevel :: State -> Int -> Either SyntaxAnalyserError (Int, Syntax)
        secondLevel _ index | index >= length tokens =
            Left $ UnexpectedEOF src 0
        
        secondLevel ExpectingOpenParenthesesOrSingleQuote index =
            case tokens !! index of
                (OpenParentheses n) ->
                    secondLevel (MakingInstantList n []) (index + 1)

                (SingleQuote _) ->
                    secondLevel ExpectingOpenParentheses (index + 1)
                
                t ->
                    unexpectedToken t "'(' or '''"
        
        secondLevel ExpectingOpenParentheses index =
            case tokens !! index of
                (OpenParentheses n) ->
                    secondLevel (MakingLazyList (n - 1) []) (index + 1)

                t ->
                    unexpectedToken t "'('"
        
        secondLevel (MakingInstantList n elems) index =
            case tokens !! index of
                (Identifier p t) ->
                    secondLevel
                        (MakingInstantList n (elems ++ [IdentifierRef p t])) (index + 1)

                (Number p t) ->
                    secondLevel
                        (MakingInstantList n (elems ++ [NumberRef p (read t)])) (index + 1)

                (StringLiteral p t) ->
                    secondLevel
                        (MakingInstantList n (elems ++ [StringRef p t])) (index + 1)

                (BoolLiteral p "t") ->
                    secondLevel
                        (MakingInstantList n (elems ++ [BoolRef p True])) (index + 1)

                (BoolLiteral p _) ->
                    secondLevel
                        (MakingInstantList n (elems ++ [BoolRef p False])) (index + 1)

                (OpenParentheses _) ->
                    secondLevel ExpectingOpenParenthesesOrSingleQuote index >>=
                        (\(newIndex, program) ->
                            secondLevel
                                (MakingInstantList n (elems ++ [program]))
                                    (newIndex + 1)
                        )
                
                (SingleQuote _) ->
                    secondLevel ExpectingOpenParenthesesOrSingleQuote index >>=
                        (\(newIndex, program) ->
                            secondLevel
                                (MakingInstantList n (elems ++ [program]))
                                    (newIndex + 1)
                        )

                (CloseParentheses _) ->
                    Right (index, InstantList n elems)

        secondLevel (MakingLazyList n elems) index =
            case tokens !! index of
                (Identifier p t) ->
                    secondLevel
                        (MakingLazyList n (elems ++ [IdentifierRef p t])) (index + 1)

                (Number p t) ->
                    secondLevel
                        (MakingLazyList n (elems ++ [NumberRef p (read t)])) (index + 1)

                (StringLiteral p t) ->
                    secondLevel
                        (MakingLazyList n (elems ++ [StringRef p t])) (index + 1)

                (BoolLiteral p "t") ->
                    secondLevel
                        (MakingLazyList n (elems ++ [BoolRef p True])) (index + 1)

                (BoolLiteral p _) ->
                    secondLevel
                        (MakingLazyList n (elems ++ [BoolRef p False])) (index + 1)

                (OpenParentheses _) ->
                    secondLevel ExpectingOpenParenthesesOrSingleQuote index >>=
                        (\(newIndex, program) ->
                            secondLevel
                                (MakingLazyList n (elems ++ [program]))
                                    (newIndex + 1)
                        )
                
                (SingleQuote _) ->
                    secondLevel ExpectingOpenParenthesesOrSingleQuote index >>=
                        (\(newIndex, program) ->
                            secondLevel
                                (MakingLazyList n (elems ++ [program]))
                                    (newIndex + 1)
                        )

                (CloseParentheses _) ->
                    Right (index, LazyList n elems)

        unexpectedToken t e =
            Left $ UnexpectedToken src (tokenIndex t) (tokenLetter t) e
