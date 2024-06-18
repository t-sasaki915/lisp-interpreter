module LispError (LispError(..)) where

import ErrorTrace (Tracable(..))

data LispError = UndefinedIdentifier Int String
               | UndefinedFunction Int String
               | TypeMismatch Int String String
               | TooFewArguments Int Int
               | TooManyArguments Int Int
               | IdentifierConfliction Int String
               | BrokenProgramStructure Int String
               deriving (Eq, Show)

instance Tracable LispError where
    place (UndefinedIdentifier a _)    = a
    place (UndefinedFunction a _)      = a
    place (TypeMismatch a _ _)         = a
    place (TooFewArguments a _)        = a
    place (TooManyArguments a _)       = a
    place (IdentifierConfliction a _)  = a
    place (BrokenProgramStructure a _) = a

    title (UndefinedIdentifier {})     = "Undefined identifier"
    title (UndefinedFunction {})       = "Undefined function"
    title (TypeMismatch {})            = "Type mismatch"
    title (TooFewArguments {})         = "Too few arguments"
    title (TooManyArguments {})        = "Too many arguments"
    title (IdentifierConfliction {})   = "Identifier confliction"
    title (BrokenProgramStructure {})  = "Broken program structure"

    cause (UndefinedIdentifier _ a)    = a
    cause (UndefinedFunction _ a)      = a
    cause (TypeMismatch _ a b)         =
        "Expected a type " ++ b ++ " but given " ++ a
    cause (TooFewArguments _ a)        =
        "This function requires at least " ++ show a ++ " arguments."
    cause (TooManyArguments _ a)       =
        "The acceptable number of arguments for this function is " ++ show a ++ "."
    cause (IdentifierConfliction _ a)  = a
    cause (BrokenProgramStructure _ a) =
        "Illegal " ++ a ++ " has found, which should not be happend."
