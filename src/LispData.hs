{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module LispData
    ( LispData(..)
    , LispState(..)
    , lispDataIndex
    , varFilt
    , varBindFilt
    , funcFilt
    , expectNumber
    , expectString
    , expectBool
    , expectIdentifier
    , expectList
    , functions
    , variables
    , localVariables
    ) where

import LispError (LispError(..))

import Control.Lens

data LispData = LispString Int String
              | LispNumber Int Int
              | LispBool Int Bool
              | LispList Int [LispData]
              | LispLazyList Int [LispData]
              | LispVariable Int String LispData
              | LispVariableBind Int String
              | LispIdentifier Int String
              | LispFunction Int String (Int -> LispState -> [LispData] ->
                             IO (Either LispError (LispState, LispData)))

data LispState = LispState
    { _functions      :: [LispData]
    , _variables      :: [LispData]
    , _localVariables :: [LispData]
    }

makeLenses ''LispState

lispDataIndex :: LispData -> Int
lispDataIndex (LispString n _)       = n
lispDataIndex (LispNumber n _)       = n
lispDataIndex (LispBool n _)         = n
lispDataIndex (LispList n _)         = n
lispDataIndex (LispLazyList n _)     = n
lispDataIndex (LispVariable n _ _)   = n
lispDataIndex (LispVariableBind n _) = n
lispDataIndex (LispIdentifier n _)   = n
lispDataIndex (LispFunction n _ _)   = n

varFilt :: String -> LispData -> Bool
varFilt name =
    \case (LispVariable _ n _) -> n == name
          _                    -> False

varBindFilt :: String -> LispData -> Bool
varBindFilt name =
    \case (LispVariableBind _ n) -> n == name
          _                      -> False

funcFilt :: String -> LispData -> Bool
funcFilt name =
    \case (LispFunction _ n _) -> n == name
          _                    -> False

expectNumber :: LispData -> Either LispError Int
expectNumber =
    \case (LispNumber _ x) -> Right x
          d -> Left $ TypeMismatch (lispDataIndex d) (show d) "Number"

expectString :: LispData -> Either LispError String
expectString =
    \case (LispString _ s) -> Right s
          d -> Left $ TypeMismatch (lispDataIndex d) (show d) "String"

expectBool :: LispData -> Either LispError Bool
expectBool =
    \case (LispBool _ b) -> Right b
          d -> Left $ TypeMismatch (lispDataIndex d) (show d) "Bool"

expectIdentifier :: LispData -> Either LispError String
expectIdentifier =
    \case (LispIdentifier _ i) -> Right i
          d -> Left $ TypeMismatch (lispDataIndex d) (show d) "Identifier"

expectList :: LispData -> Either LispError [LispData]
expectList =
    \case (LispList _ l) -> Right l
          d -> Left $ TypeMismatch (lispDataIndex d) (show d) "List"

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
    show (LispIdentifier _ i) =
        "identifier \"" ++ i ++ "\""
    show (LispFunction _ l _) =
        "function \"" ++ l ++ "\""

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
    (==) (LispIdentifier n1 i1) (LispIdentifier n2 i2) =
        n1 == n2 && i1 == i2
    (==) (LispFunction n1 l1 _) (LispFunction n2 l2 _) =
        n1 == n2 && l1 == l2
    (==) _ _ =
        False
