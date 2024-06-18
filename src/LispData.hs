module LispData (LispData(..), LispState(..)) where

import LispError (LispError(..))
import Syntax (Syntax(..))

data LispData = LispString Int String
              | LispNumber Int Int
              | LispBool Int Bool
              | LispList Int [Syntax]
              | LispLazyList Int [Syntax]
              | LispVariable Int String LispData
              | LispVariableBind Int String
              | LispFunction Int String ([LispData] -> LispState ->
                             IO (Either LispError (LispState, LispData)))
              | LispSyntax Int String ([Syntax] -> LispState ->
                           IO (Either LispError (LispState, LispData)))

data LispState = LispState
    { _syntaxes       :: [LispData]
    , _functions      :: [LispData]
    , _variables      :: [LispData]
    , _localVariables :: [LispData]
    }
    deriving (Eq, Show)

instance Show LispData where
    show (LispString _ s) =
        "\"" ++ s ++ "\""
    show (LispNumber _ x) =
        show x
    show (LispBool _ True) =
        "t"
    show (LispBool _ False) =
        "nil"
    show (LispList _ l) =
        "(" ++ unwords (map show l) ++ ")"
    show (LispLazyList _ l) =
        "'(" ++ unwords (map show l) ++ ")"
    show (LispVariable _ l d) =
        l ++ " := " ++ show d
    show (LispVariableBind _ l) =
        l ++ " := ???"
    show (LispFunction _ l _) =
        "function \"" ++ l ++ "\""
    show (LispSyntax _ l _) =
        "syntax \"" ++ l ++ "\""

instance Eq LispData where
    (==) (LispString n1 s1) (LispString n2 s2) =
        n1 == n2 && s1 == s2
    (==) (LispNumber n1 x1) (LispNumber n2 x2) =
        n1 == n2 && x1 == x2
    (==) (LispBool n1 b1) (LispBool n2 b2) =
        n1 == n2 && b1 == b2
    (==) (LispList n1 l1) (LispList n2 l2) =
        n1 == n2 && l1 == l2
    (==) (LispLazyList n1 l1) (LispLazyList n2 l2) =
        n1 == n2 && l1 == l2
    (==) (LispVariable n1 l1 d1) (LispVariable n2 l2 d2) =
        n1 == n2 && l1 == l2 && d1 == d2
    (==) (LispVariableBind n1 l1) (LispVariableBind n2 l2) =
        n1 == n2 && l1 == l2
    (==) (LispFunction n1 l1 _) (LispFunction n2 l2 _) =
        n1 == n2 && l1 == l2
    (==) (LispSyntax n1 l1 _) (LispSyntax n2 l2 _) =
        n1 == n2 && l1 == l2
    (==) _ _ =
        False
