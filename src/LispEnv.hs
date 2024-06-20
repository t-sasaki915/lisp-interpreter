{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispEnv where

import LispData (LispData(..), index)
import LispError (RuntimeError(..))

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT, get)
import Data.Functor ((<&>))
import Data.List (find)

type Eval = ExceptT RuntimeError (StateT LispEnv IO) LispData

data LispEnvData = LispFunction (Int -> [LispData] -> Eval)
                 | LispVariable LispData

type LispEnv = [(String, LispEnvData)]

isVariable :: (String, LispEnvData) -> Bool
isVariable (_, LispVariable _) = True
isVariable _                   = False

isFunction :: (String, LispEnvData) -> Bool
isFunction (_, LispFunction _) = True
isFunction _                   = False

variableReference :: LispData -> Eval
variableReference (LispSymbol n label) = do
    vars <- lift (get <&> filter isVariable)
    case find (\(l, _) -> l == label) vars of
        Just (_, LispVariable d) ->
            return d
        _ ->
            throwE (UndefinedVariable n label)

variableReference x = throwE (IllegalBehaviour (index x))

functionReference :: LispData -> ExceptT RuntimeError
                                 (StateT LispEnv IO)
                                 (Int -> [LispData] -> Eval)
functionReference (LispSymbol n label) = do
    funs <- lift (get <&> filter isFunction)
    case find (\(l, _) -> l == label) funs of
        Just (_, LispFunction f) ->
            return f
        _ ->
            throwE (UndefinedFunction n label)

functionReference x = throwE (IllegalBehaviour (index x))
