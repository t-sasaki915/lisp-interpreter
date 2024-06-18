module LispError (LispError(..)) where

import ErrorTrace (Tracable(..))

data LispError = UndefinedIdentifier Int String
               | UndefinedFunction Int String
               | TypeMismatch Int String String
               | UninitialisedVariableAccess Int String
               | BrokenProgramStructure Int String
               deriving (Eq, Show)

instance Tracable LispError where
    place (UndefinedIdentifier a _)         = a
    place (UndefinedFunction a _)           = a
    place (TypeMismatch a _ _)              = a
    place (UninitialisedVariableAccess a _) = a
    place (BrokenProgramStructure a _)      = a

    title (UndefinedIdentifier {})          = "Undefined identifier"
    title (UndefinedFunction {})            = "Undefined function"
    title (TypeMismatch {})                 = "Type mismatch"
    title (UninitialisedVariableAccess {})  =
        "Uninitialised variable access"
    title (BrokenProgramStructure {})       = "Broken program structure"

    cause (UndefinedIdentifier _ a)         = a
    cause (UndefinedFunction _ a)           = a
    cause (TypeMismatch _ a b)              =
        "Expected a type " ++ b ++ " but given " ++ a
    cause (UninitialisedVariableAccess _ a) =
        "Variable " ++ a ++ " is uninitialised."
    cause (BrokenProgramStructure _ a)      =
        "Illegal " ++ a ++ " has found, which should not be happend."
