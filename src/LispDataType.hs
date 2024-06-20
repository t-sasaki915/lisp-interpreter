module LispDataType (LispDataType(..)) where

data LispDataType = LispInteger Int Int
                  | LispReal Int Float
                  | LispRational Int Int Int
                  | LispSymbol Int String
                  | LispBoolean Int Bool
                  | LispString Int String
                  | LispCharacter Int Char
                  | LispList Int [LispDataType]
                  deriving (Eq, Show)
