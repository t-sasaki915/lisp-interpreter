module SyntaxAnalyser
    ( SyntaxAnalyserError(..)
    , LispSyntax(..)
    , syntaxAnalyse
    ) where

import ErrorTrace (Tracable(..))
import Token (Token(..), tokenIndex, tokenLetter)

data SyntaxAnalyserError = UnexpectedToken String Int String String
                         | UnexpectedEOF String Int

instance Tracable SyntaxAnalyserError where
    place (UnexpectedToken a b _ _) = (a, b)
    place (UnexpectedEOF a b)       = (a, b)

    title (UnexpectedToken {})      = "Unexpected token"
    title (UnexpectedEOF {})        = "Unexpected end of file"

    cause (UnexpectedToken _ _ a b) =
        "The interpreter was expecting " ++ b ++ ", but what found was " ++ a ++ "."
    cause (UnexpectedEOF _ _)       = ""

data LispSyntax = VarValue Token
                | NumValue Token
                | StrValue Token
                | ExecList [LispSyntax]
                | RawList [LispSyntax]
                deriving (Eq, Show)

data State = ExpectingOpenParenthesesOrSingleQuote
           | ExpectingOpenParentheses 
           | MakingExecList [LispSyntax]
           | MakingRawList [LispSyntax]

syntaxAnalyse :: String -> [Token] -> Either SyntaxAnalyserError [LispSyntax]
syntaxAnalyse src tokens = topLevel ExpectingOpenParenthesesOrSingleQuote 0 []
    where
        topLevel :: State -> Int -> [LispSyntax] ->
                    Either SyntaxAnalyserError [LispSyntax]
        topLevel state index determined | index >= length tokens =
            case state of
                ExpectingOpenParenthesesOrSingleQuote ->
                    Right determined

                _ ->
                    Left $ UnexpectedEOF src 0
        
        topLevel ExpectingOpenParenthesesOrSingleQuote index determined =
            let token = tokens !! index in case token of
                (OpenParentheses _) ->
                    topLevel (MakingExecList []) index determined
            
                (SingleQuote _) ->
                    topLevel ExpectingOpenParentheses (index + 1) determined

                t ->
                    unexpectedToken t "'(' or '''"
        
        topLevel ExpectingOpenParentheses index determined =
            let token = tokens !! index in case token of
                (OpenParentheses _) ->
                    topLevel (MakingRawList []) (index - 1) determined

                t ->
                    unexpectedToken t "'('"
        
        topLevel (MakingExecList _) index determined =
            secondLevel ExpectingOpenParenthesesOrSingleQuote index >>=
                (\(newIndex, program) ->
                    topLevel
                        ExpectingOpenParenthesesOrSingleQuote
                            (newIndex + 1)
                                (determined ++ [program])
                )

        topLevel (MakingRawList _) index determined =
            secondLevel ExpectingOpenParenthesesOrSingleQuote index >>=
                (\(newIndex, program) ->
                    topLevel
                        ExpectingOpenParenthesesOrSingleQuote
                            (newIndex + 1)
                                (determined ++ [program])
                )

        secondLevel :: State -> Int -> Either SyntaxAnalyserError (Int, LispSyntax)
        secondLevel _ index | index >= length tokens =
            Left $ UnexpectedEOF src 0
        
        secondLevel ExpectingOpenParenthesesOrSingleQuote index =
            let token = tokens !! index in case token of
                (OpenParentheses _) ->
                    secondLevel (MakingExecList []) (index + 1)

                (SingleQuote _) ->
                    secondLevel ExpectingOpenParentheses (index + 1)
                
                t ->
                    unexpectedToken t "'(' or '''"
        
        secondLevel ExpectingOpenParentheses index =
            let token = tokens !! index in case token of
                (OpenParentheses _) ->
                    secondLevel (MakingRawList []) (index + 1)

                t ->
                    unexpectedToken t "'('"
        
        secondLevel (MakingExecList elems) index =
            let token = tokens !! index in case token of
                (Identifier _ _) ->
                    secondLevel (MakingExecList (elems ++ [VarValue token])) (index + 1)

                (Number _ _) ->
                    secondLevel (MakingExecList (elems ++ [NumValue token])) (index + 1)

                (StringLiteral _ _) ->
                    secondLevel (MakingExecList (elems ++ [StrValue token])) (index + 1)

                (OpenParentheses _) ->
                    secondLevel ExpectingOpenParenthesesOrSingleQuote index >>=
                        (\(newIndex, program) ->
                            secondLevel
                                (MakingExecList (elems ++ [program]))
                                    (newIndex + 1)
                        )
                
                (SingleQuote _) ->
                    secondLevel ExpectingOpenParenthesesOrSingleQuote index >>=
                        (\(newIndex, program) ->
                            secondLevel
                                (MakingExecList (elems ++ [program]))
                                    (newIndex + 1)
                        )

                (CloseParentheses _) ->
                    Right (index, ExecList elems)

        secondLevel (MakingRawList elems) index =
            let token = tokens !! index in case token of
                (Identifier _ _) ->
                    secondLevel (MakingRawList (elems ++ [VarValue token])) (index + 1)

                (Number _ _) ->
                    secondLevel (MakingRawList (elems ++ [NumValue token])) (index + 1)

                (StringLiteral _ _) ->
                    secondLevel (MakingRawList (elems ++ [StrValue token])) (index + 1)

                (OpenParentheses _) ->
                    secondLevel ExpectingOpenParenthesesOrSingleQuote index >>=
                        (\(newIndex, program) ->
                            secondLevel
                                (MakingRawList (elems ++ [program]))
                                    (newIndex + 1)
                        )
                
                (SingleQuote _) ->
                    secondLevel ExpectingOpenParenthesesOrSingleQuote index >>=
                        (\(newIndex, program) ->
                            secondLevel
                                (MakingRawList (elems ++ [program]))
                                    (newIndex + 1)
                        )

                (CloseParentheses _) ->
                    Right (index, RawList elems)

        unexpectedToken t e = Left $ UnexpectedToken src (tokenIndex t) (tokenLetter t) e
