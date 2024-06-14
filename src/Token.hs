module Token (Token(..)) where

data Token = OpenParentheses Int
           | CloseParentheses Int
           | SingleQuote Int
           | Symbol Int String
           | Identifier Int String
           | StringLiteral Int String
           | Number Int String
           deriving Eq

instance Show Token where
    show (OpenParentheses _)  = "("
    show (CloseParentheses _) = ")"
    show (SingleQuote _)      = "'"
    show (Symbol _ a)         = "Symbol " ++ a
    show (Identifier _ a)     = "Identifier " ++ a
    show (StringLiteral _ a)  = "String " ++ a
    show (Number _ a)         = "Number " ++ a
