module Tokeniser (TokeniserError(..), tokenise) where

import ErrorTrace (Tracable(..))
import Token (Token(..))

data TokeniserError = UnrecognisableCharacter String Int Char
                    | UnexpectedEOF String Int
                    deriving (Show, Eq)
    
instance Tracable TokeniserError where
    place (UnrecognisableCharacter a b _) = (a, b)
    place (UnexpectedEOF a b)             = (a, b)

    title (UnrecognisableCharacter {})    = "Unrecognisable character"
    title (UnexpectedEOF {})              = "Unexpected end of file"

    cause (UnrecognisableCharacter _ _ a) = [a, '.']
    cause (UnexpectedEOF {})              = ""

tokenise :: String -> Either TokeniserError [Token]
tokenise src = Right []
