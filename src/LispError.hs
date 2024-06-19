module LispError (LispError(..)) where

import ErrorTrace (Tracable(..))

data LispError = UndefinedIdentifier Int String
               | UndefinedFunction Int String
               | TypeMismatch Int String String
               | TooFewArguments Int Int
               | TooManyArguments Int Int
               | IdentifierConfliction Int String
               | UnexpectedEndOfString Int
               | IndentNotAllowed Int Char
               | UnknownFormatCommand Int Char
               | IllegalArgument Int String
               deriving (Eq, Show)

instance Tracable LispError where
    place (UndefinedIdentifier a _)    = a
    place (UndefinedFunction a _)      = a
    place (TypeMismatch a _ _)         = a
    place (TooFewArguments a _)        = a
    place (TooManyArguments a _)       = a
    place (IdentifierConfliction a _)  = a
    place (UnexpectedEndOfString a)    = a
    place (IndentNotAllowed a _)       = a
    place (UnknownFormatCommand a _)   = a
    place (IllegalArgument a _)        = a

    title (UndefinedIdentifier {})     = "Undefined identifier"
    title (UndefinedFunction {})       = "Undefined function"
    title (TypeMismatch {})            = "Type mismatch"
    title (TooFewArguments {})         = "Too few arguments"
    title (TooManyArguments {})        = "Too many arguments"
    title (IdentifierConfliction {})   = "Identifier confliction"
    title (UnexpectedEndOfString {})   = "Unexpected end of string"
    title (IndentNotAllowed {})        = "Indent not allowed"
    title (UnknownFormatCommand {})    = "Unknown format command"
    title (IllegalArgument {})         = "Illegal argument"

    cause (UndefinedIdentifier _ a)    = a
    cause (UndefinedFunction _ a)      = a
    cause (TypeMismatch _ a b)         =
        "Expected a type " ++ b ++ " but given " ++ a
    cause (TooFewArguments _ a)        =
        "This function requires at least " ++ show a ++ " arguments."
    cause (TooManyArguments _ a)       =
        "The acceptable number of arguments for this function is " ++ show a ++ "."
    cause (IdentifierConfliction _ a)  = a
    cause (UnexpectedEndOfString _)    = ""
    cause (IndentNotAllowed _ a)       = [a]
    cause (UnknownFormatCommand _ a)   = [a]
    cause (IllegalArgument _ a)        = a
