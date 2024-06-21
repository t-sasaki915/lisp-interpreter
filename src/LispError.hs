module LispError
    ( ParseError(..)
    , RuntimeError(..)
    , traceError
    ) where

class LispError a where
    index :: a -> Int
    title :: a -> String
    cause :: a -> String

data ParseError = UnexpectedEOF
                | UnknownChar Int String
                deriving (Eq, Show)

instance LispError ParseError where
    index UnexpectedEOF     = 0
    index (UnknownChar n _) = n

    title UnexpectedEOF     = "Unexpected end of file"
    title (UnknownChar {})  = "Unknown character"

    cause UnexpectedEOF     = ""
    cause (UnknownChar _ a) = a

data RuntimeError = UndefinedVariable Int String
                  | UndefinedFunction Int String
                  | TooManyArguments Int String Int
                  | TooFewArguments Int String Int
                  | IncompatibleType Int String String
                  | IllegalFunctionCall Int
                  | IllegalBehaviour Int
                  deriving (Eq, Show)

instance LispError RuntimeError where
    index (UndefinedVariable a _)  = a
    index (UndefinedFunction a _)  = a
    index (TooManyArguments a _ _) = a
    index (TooFewArguments a _ _)  = a
    index (IncompatibleType a _ _) = a
    index (IllegalFunctionCall a)  = a
    index (IllegalBehaviour a)     = a

    title (UndefinedVariable {})   = "Undefined variable"
    title (UndefinedFunction {})   = "Undefined function"
    title (TooManyArguments {})    = "Too many arguments"
    title (TooFewArguments {})     = "Too few arguments"
    title (IncompatibleType {})    = "Incompatible type"
    title (IllegalFunctionCall {}) = "Illegal function call"
    title (IllegalBehaviour {})    = "Illegal interpreter behaviour"

    cause (UndefinedVariable _ a)  = a
    cause (UndefinedFunction _ a)  = a
    cause (TooManyArguments _ a b) =
        a ++ " requires only " ++ show b ++ " arguments."
    cause (TooFewArguments _ a b) =
        a ++ " requires at least " ++ show b ++ " arguments."
    cause (IncompatibleType _ a b) =
        a ++ " and " ++ b ++ " are incompatible."
    cause (IllegalFunctionCall _)  = ""
    cause (IllegalBehaviour _)     = ""

traceError :: (LispError a) => String -> a -> String
traceError src err =
    "(Line " ++ line ++ ", Index " ++ ind ++ ") " ++ title err ++
        case cause err of
            "" -> "."
            c  -> ": " ++ c
    where
        line = show $ length (lines (take (index err + 1) src))
        ind = show $ subtract 1 (length (last (lines (take (index err + 1) src))))
