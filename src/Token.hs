module Token (Token(..)) where

data Token = OpenParentheses Int
           | CloseParentheses Int
           | SingleQuote Int
           | Identifier Int String
           | StringLiteral Int String
           | Number Int String
           deriving Eq

instance Show Token where
    show (OpenParentheses n)  = "(" ++ show n ++ ", \"(\")"
    show (CloseParentheses n) = "(" ++ show n ++ ", \")\")"
    show (SingleQuote n)      = "(" ++ show n ++ ", \"'\")"
    show (Identifier n a)     = "(" ++ show n ++ ", Identifier \"" ++ a ++ "\")"
    show (StringLiteral n a)  = "(" ++ show n ++ ", String \"" ++ a ++ "\")"
    show (Number n a)         = "(" ++ show n ++ ", Number \"" ++ a ++ "\")"
