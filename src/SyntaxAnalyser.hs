module SyntaxAnalyser
    ( SyntaxAnalyserError(..)
    , syntaxAnalyse
    ) where

import ErrorTrace (Tracable(..))
import Syntax (Syntax(..))
import Token (Token(..), tokenIndex, tokenLetter)

data SyntaxAnalyserError = UnexpectedToken Int String String
                         | UnexpectedEOF Int
                         deriving (Eq, Show)

instance Tracable SyntaxAnalyserError where
    place (UnexpectedToken a _ _) = a
    place (UnexpectedEOF a)       = a

    title (UnexpectedToken {})    = "Unexpected token"
    title (UnexpectedEOF {})      = "Unexpected end of file"

    cause (UnexpectedToken _ a b) =
        "The interpreter was expecting " ++ b ++ ", but what found was " ++ a ++ "."
    cause (UnexpectedEOF _)       = ""

data State = ExpectingOpenParenthesesOrSingleQuote
           | ExpectingOpenParentheses 
           | MakingInstantList [Syntax]
           | MakingLazyList [Syntax]

syntaxAnalyse :: [Token] -> Either SyntaxAnalyserError [Syntax]
syntaxAnalyse tokens = topLevel ExpectingOpenParenthesesOrSingleQuote 0 []
    where
        topLevel :: State -> Int -> [Syntax] ->
                    Either SyntaxAnalyserError [Syntax]
        topLevel state index determined | index >= length tokens =
            case state of
                ExpectingOpenParenthesesOrSingleQuote ->
                    Right determined

                _ ->
                    Left $ UnexpectedEOF 0
        
        topLevel ExpectingOpenParenthesesOrSingleQuote index determined =
            case tokens !! index of
                (OpenParentheses _) ->
                    topLevel (MakingInstantList []) index determined
            
                (SingleQuote _) ->
                    topLevel ExpectingOpenParentheses (index + 1) determined

                t ->
                    unexpectedToken t "'(' or '''"
        
        topLevel ExpectingOpenParentheses index determined =
            case tokens !! index of
                (OpenParentheses _) ->
                    topLevel (MakingLazyList []) (index - 1) determined

                t ->
                    unexpectedToken t "'('"
        
        topLevel (MakingInstantList _) index determined =
            secondLevel ExpectingOpenParenthesesOrSingleQuote index >>=
                (\(newIndex, program) ->
                    topLevel
                        ExpectingOpenParenthesesOrSingleQuote
                            (newIndex + 1)
                                (determined ++ [program])
                )

        topLevel (MakingLazyList _) index determined =
            secondLevel ExpectingOpenParenthesesOrSingleQuote index >>=
                (\(newIndex, program) ->
                    topLevel
                        ExpectingOpenParenthesesOrSingleQuote
                            (newIndex + 1)
                                (determined ++ [program])
                )

        secondLevel :: State -> Int -> Either SyntaxAnalyserError (Int, Syntax)
        secondLevel _ index | index >= length tokens =
            Left $ UnexpectedEOF 0
        
        secondLevel ExpectingOpenParenthesesOrSingleQuote index =
            case tokens !! index of
                (OpenParentheses _) ->
                    secondLevel (MakingInstantList []) (index + 1)

                (SingleQuote _) ->
                    secondLevel ExpectingOpenParentheses (index + 1)
                
                t ->
                    unexpectedToken t "'(' or '''"
        
        secondLevel ExpectingOpenParentheses index =
            case tokens !! index of
                (OpenParentheses _) ->
                    secondLevel (MakingLazyList []) (index + 1)

                t ->
                    unexpectedToken t "'('"
        
        secondLevel (MakingInstantList elems) index =
            case tokens !! index of
                (Identifier p t) ->
                    secondLevel
                        (MakingInstantList (elems ++ [IdentifierRef p t])) (index + 1)

                (Number p t) ->
                    secondLevel
                        (MakingInstantList (elems ++ [NumberRef p (read t)])) (index + 1)

                (StringLiteral p t) ->
                    secondLevel
                        (MakingInstantList (elems ++ [StringRef p t])) (index + 1)

                (BoolLiteral p "t") ->
                    secondLevel
                        (MakingInstantList (elems ++ [BoolRef p True])) (index + 1)

                (BoolLiteral p _) ->
                    secondLevel
                        (MakingInstantList (elems ++ [BoolRef p False])) (index + 1)

                (OpenParentheses _) ->
                    secondLevel ExpectingOpenParenthesesOrSingleQuote index >>=
                        (\(newIndex, program) ->
                            secondLevel
                                (MakingInstantList (elems ++ [program]))
                                    (newIndex + 1)
                        )
                
                (SingleQuote _) ->
                    secondLevel ExpectingOpenParenthesesOrSingleQuote index >>=
                        (\(newIndex, program) ->
                            secondLevel
                                (MakingInstantList (elems ++ [program]))
                                    (newIndex + 1)
                        )

                (CloseParentheses n) ->
                    Right (index, InstantList n elems)

        secondLevel (MakingLazyList elems) index =
            case tokens !! index of
                (Identifier p t) ->
                    secondLevel
                        (MakingLazyList (elems ++ [IdentifierRef p t])) (index + 1)

                (Number p t) ->
                    secondLevel
                        (MakingLazyList (elems ++ [NumberRef p (read t)])) (index + 1)

                (StringLiteral p t) ->
                    secondLevel
                        (MakingLazyList (elems ++ [StringRef p t])) (index + 1)

                (BoolLiteral p "t") ->
                    secondLevel
                        (MakingLazyList (elems ++ [BoolRef p True])) (index + 1)

                (BoolLiteral p _) ->
                    secondLevel
                        (MakingLazyList (elems ++ [BoolRef p False])) (index + 1)

                (OpenParentheses _) ->
                    secondLevel ExpectingOpenParenthesesOrSingleQuote index >>=
                        (\(newIndex, program) ->
                            secondLevel
                                (MakingLazyList (elems ++ [program]))
                                    (newIndex + 1)
                        )
                
                (SingleQuote _) ->
                    secondLevel ExpectingOpenParenthesesOrSingleQuote index >>=
                        (\(newIndex, program) ->
                            secondLevel
                                (MakingLazyList (elems ++ [program]))
                                    (newIndex + 1)
                        )

                (CloseParentheses n) ->
                    Right (index, LazyList n elems)

        unexpectedToken t e =
            Left $ UnexpectedToken (tokenIndex t) (tokenLetter t) e
