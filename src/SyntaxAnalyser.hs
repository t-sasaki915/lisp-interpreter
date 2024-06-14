module SyntaxAnalyser
    ( SyntaxAnalyserError(..)
    , LispSyntax(..)
    , syntaxAnalyse
    ) where

import ErrorTrace (Tracable(..))
import Token (Token(..))

data SyntaxAnalyserError = UnexpectedEOF String Int

instance Tracable SyntaxAnalyserError where
    place (UnexpectedEOF a b) = (a, b)

    title (UnexpectedEOF {})  = "Unexpected end of file"

    cause (UnexpectedEOF _ _) = ""

data LispSyntax = VarValue Token
                | NumValue Token
                | FunValue Token
                | ExecList [LispSyntax]
                | RawList [LispSyntax]
                deriving (Eq, Show)

syntaxAnalyse :: String -> [Token] -> Either SyntaxAnalyserError [LispSyntax]
syntaxAnalyse src tokens = Right []
