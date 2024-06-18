module LispError (LispError(..)) where

import ErrorTrace (Tracable(..))

data LispError = UndefinedIdentifier String Int String
               | UndefinedFunction String Int String
               deriving (Eq, Show)

instance Tracable LispError where
    place (UndefinedIdentifier a b _) = (a, b)
    place (UndefinedFunction a b _)   = (a, b)

    title (UndefinedIdentifier {})    = "Undefined identifier"
    title (UndefinedFunction {})      = "Undefined function"

    cause (UndefinedIdentifier _ _ a) = a
    cause (UndefinedFunction _ _ a)   = a
