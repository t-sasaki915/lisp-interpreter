module LispError (ParseError(..), RuntimeError(..)) where

data ParseError = UnexpectedEOF
                | UnknownChar String
                | UnexpectedToken Char
                | ZeroDivideCalculation'
                deriving Eq

instance Show ParseError where
    show UnexpectedEOF          = "Unexpected end of file."
    show (UnknownChar a)        = "Unknown character: " ++ a
    show (UnexpectedToken a)    = "Unexpected token: " ++ [a]
    show ZeroDivideCalculation' = "Zero divide calculation."

data RuntimeError = UndefinedVariable String
                  | UndefinedFunction String
                  | TooManyArguments String Int
                  | TooFewArguments String Int
                  | IncompatibleType String String
                  | ZeroDivideCalculation
                  | UninitialisedVariable String
                  | IllegalFunctionCall
                  deriving Eq

instance Show RuntimeError where
    show (UndefinedVariable a) =
        "Variable " ++ a ++ " is undefined."
    show (UndefinedFunction a) =
        "Function " ++ a ++ " is undefined."
    show (TooManyArguments a b) =
        a ++ " requires only " ++ show b ++ " arguments."
    show (TooFewArguments a b) =
        a ++ " requires at least " ++ show b ++ " arguments."
    show (IncompatibleType a b) =
        "Types " ++ a ++ " and " ++ b ++ " are incompatible."
    show ZeroDivideCalculation =
        "Zero divide calculation."
    show (UninitialisedVariable a) =
        "Variable " ++ a ++ " has not initialised yet."
    show IllegalFunctionCall =
        "Illegal function call."
