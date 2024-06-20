{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispData where

data LispData = LispInteger Int Int
              | LispReal Int Float
              | LispRational Int Int Int
              | LispSymbol Int String
              | LispBool Int Bool
              | LispString Int String
              | LispCharacter Int Char
              | LispList Int [LispData]
              | LispQuote LispData
              deriving Eq

treatAsLispBool :: LispData -> Bool
treatAsLispBool (LispList _ [])    = False
treatAsLispBool (LispBool _ False) = False
treatAsLispBool _                  = True

isAtom :: LispData -> Bool
isAtom (LispList _ _) = False
isAtom _              = True

isList :: LispData -> Bool
isList (LispList _ _) = True
isList _              = False

index :: LispData -> Int
index (LispInteger n _)    = n
index (LispReal n _)       = n
index (LispRational n _ _) = n
index (LispSymbol n _)     = n
index (LispBool n _)       = n
index (LispString n _)     = n
index (LispCharacter n _)  = n
index (LispList n _)       = n
index (LispQuote d)        = index d

instance Show LispData where
    show (LispInteger _ n)    = show n
    show (LispReal _ n)       = show n
    show (LispRational _ a b) = show a ++ "/" ++ show b
    show (LispSymbol _ n)     = n
    show (LispBool _ True)    = "#T"
    show (LispBool _ False)   = "#F"
    show (LispString _ s)     = "\"" ++ s ++ "\""
    show (LispCharacter _ c)  = "#\\" ++ [c]
    show (LispList _ l)       = "(" ++ unwords (map show l) ++ ")"
    show (LispQuote d)        = "'" ++ show d
