{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module LispData where

import ExceptExtra (exceptT)
import LispError (LispError(..))

import Control.Lens (makeLenses)
import Control.Monad.Trans.Except (Except, ExceptT, throwE)

data LispData = LispString Int String
              | LispNumber Int Int
              | LispBool Int Bool
              | LispList Int [LispData]
              | LispLazyList Int [LispData]
              | LispVariable Int String LispData
              | LispIdentifier Int String
              | LispFunction Int String (Int -> LispState -> [LispData] ->
                             ExceptT LispError IO (LispState, LispData))

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
lispDataIndex (LispIdentifier n _)   = n
lispDataIndex (LispFunction n _ _)   = n

varFilt :: String -> LispData -> Bool
varFilt name =
    \case (LispVariable _ n _) -> n == name
          _                    -> False

funcFilt :: String -> LispData -> Bool
funcFilt name =
    \case (LispFunction _ n _) -> n == name
          _                    -> False

expectNumber :: LispData -> Except LispError Int
expectNumber =
    \case (LispNumber _ x) -> return x
          d -> throwE $ TypeMismatch (lispDataIndex d) (show d) "Number"

expectString :: LispData -> Except LispError String
expectString =
    \case (LispString _ s) -> return s
          d -> throwE $ TypeMismatch (lispDataIndex d) (show d) "String"

expectBool :: LispData -> Except LispError Bool
expectBool =
    \case (LispBool _ b) -> return b
          (LispList _ []) -> return False
          (LispLazyList _ []) -> return False
          d -> throwE $ TypeMismatch (lispDataIndex d) (show d) "Bool"

expectIdentifier :: LispData -> Except LispError String
expectIdentifier =
    \case (LispIdentifier _ i) -> return i
          d -> throwE $ TypeMismatch (lispDataIndex d) (show d) "Identifier"

expectList :: LispData -> Except LispError [LispData]
expectList =
    \case (LispList _ l)     -> return l
          (LispBool _ False) -> return []
          d -> throwE $ TypeMismatch (lispDataIndex d) (show d) "List"

expectLazyList :: LispData -> Except LispError [LispData]
expectLazyList =
    \case (LispLazyList _ l) -> return l
          (LispBool _ False) -> return []
          d -> throwE $ TypeMismatch (lispDataIndex d) (show d) "Lazy List"

expectNumberT :: (Monad m) => LispData -> ExceptT LispError m Int
expectNumberT = exceptT . expectNumber

expectStringT :: (Monad m) => LispData -> ExceptT LispError m String
expectStringT = exceptT . expectString

expectBoolT :: (Monad m) => LispData -> ExceptT LispError m Bool
expectBoolT = exceptT . expectBool

expectIdentifierT :: (Monad m) => LispData -> ExceptT LispError m String
expectIdentifierT = exceptT . expectIdentifier

expectListT :: (Monad m) => LispData -> ExceptT LispError m [LispData]
expectListT = exceptT . expectList

expectLazyListT :: (Monad m) => LispData -> ExceptT LispError m [LispData]
expectLazyListT = exceptT . expectLazyList

instance Show LispData where
    show (LispString _ s) =
        "\"" ++ s ++ "\""
    show (LispNumber _ x) =
        show x
    show (LispBool _ True) =
        "t"
    show (LispBool _ False) =
        "nil"
    show (LispList _ []) =
        "nil"
    show (LispList _ l) =
        "(" ++ unwords (map show l) ++ ")"
    show (LispLazyList _ []) =
        "nil"
    show (LispLazyList _ l) =
        "'(" ++ unwords (map show l) ++ ")"
    show (LispVariable _ l d) =
        l ++ " := " ++ show d
    show (LispIdentifier _ i) =
        "Identifier \"" ++ i ++ "\""
    show (LispFunction _ l _) =
        "Function \"" ++ l ++ "\""

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
    (==) (LispIdentifier n1 i1) (LispIdentifier n2 i2) =
        n1 == n2 && i1 == i2
    (==) (LispFunction n1 l1 _) (LispFunction n2 l2 _) =
        n1 == n2 && l1 == l2
    (==) _ _ =
        False
