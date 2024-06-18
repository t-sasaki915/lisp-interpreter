module Syntax (Syntax(..)) where

data Syntax = IdentifierRef Int String
            | NumberRef Int Int
            | StringRef Int String
            | BoolRef Int Bool
            | InstantList Int [Syntax]
            | LazyList Int [Syntax]
            deriving (Eq, Show)
