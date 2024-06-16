module Token
    ( Token(..)
    , tokenIndex
    , tokenLetter
    ) where

data Token = OpenParentheses Int
           | CloseParentheses Int
           | SingleQuote Int
           | Identifier Int String
           | StringLiteral Int String
           | BoolLiteral Int String
           | Number Int String
           deriving Eq

instance Show Token where
    show (OpenParentheses n)  = "(" ++ show n ++ ", '(')"
    show (CloseParentheses n) = "(" ++ show n ++ ", ')')"
    show (SingleQuote n)      = "(" ++ show n ++ ", ''')"
    show (Identifier n a)     = "(" ++ show n ++ ", Identifier '" ++ a ++ "')"
    show (StringLiteral n a)  = "(" ++ show n ++ ", String '" ++ a ++ "')"
    show (BoolLiteral n a)    = "(" ++ show n ++ ", Bool '" ++ a ++ "')"
    show (Number n a)         = "(" ++ show n ++ ", Number '" ++ a ++ "')"

tokenIndex :: Token -> Int
tokenIndex (OpenParentheses n)  = n
tokenIndex (CloseParentheses n) = n
tokenIndex (SingleQuote n)      = n
tokenIndex (Identifier n _)     = n
tokenIndex (StringLiteral n _)  = n
tokenIndex (BoolLiteral n _)    = n
tokenIndex (Number n _)         = n

tokenLetter :: Token -> String
tokenLetter (OpenParentheses _)  = "'('"
tokenLetter (CloseParentheses _) = "')'"
tokenLetter (SingleQuote _)      = "'''"
tokenLetter (Identifier _ a)     = "Identifier '" ++ a ++ "'"
tokenLetter (StringLiteral _ a)  = "String '" ++ a ++ "'"
tokenLetter (BoolLiteral _ a)    = "Bool '" ++ a ++ "'"
tokenLetter (Number _ a)         = "Number '" ++ a ++ "'"
