{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispEnv where

import LispData (LispData(..), index)
import LispError (RuntimeError(..))

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT, get)
import Data.Functor ((<&>))
import Data.List (find)

type EvalT a  = ExceptT RuntimeError (StateT LispEnv IO) a
type Eval     = EvalT LispData
type Evalable = Int -> [LispData] -> Eval

data LispEnvData = LispProcedure Evalable
                 | LispSyntax Evalable
                 | LispVariable LispData

type LispEnv = [(String, LispEnvData)]

isVariable :: (String, LispEnvData) -> Bool
isVariable (_, LispVariable _) = True
isVariable _                   = False

isSyntax :: (String, LispEnvData) -> Bool
isSyntax (_, LispSyntax _) = True
isSyntax _                 = False

isProcedure :: (String, LispEnvData) -> Bool
isProcedure (_, LispProcedure _) = True
isProcedure _                    = False

syntaxLabels :: EvalT [String]
syntaxLabels = do
    syntaxes <- lift (get <&> filter isSyntax)
    return (map fst syntaxes)

variableReference :: LispData -> Eval
variableReference (LispSymbol n label) = do
    vars <- lift (get <&> filter isVariable)
    case find (\(l, _) -> l == label) vars of
        Just (_, LispVariable d) ->
            return d
        _ ->
            throwE (UndefinedVariable n label)

variableReference x = throwE (IllegalBehaviour (index x))

syntaxReference :: LispData -> EvalT Evalable
syntaxReference (LispSymbol n label) = do
    syntaxes <- lift (get <&> filter isSyntax)
    case find (\(l, _) -> l == label) syntaxes of
        Just (_, LispSyntax f) ->
            return f
        _ ->
            throwE (IllegalBehaviour n)

syntaxReference x = throwE (IllegalBehaviour (index x))

procedureReference :: LispData -> EvalT Evalable
procedureReference (LispSymbol n label) = do
    funs <- lift (get <&> filter isProcedure)
    case find (\(l, _) -> l == label) funs of
        Just (_, LispProcedure f) ->
            return f
        _ ->
            throwE (UndefinedFunction n label)

procedureReference x = throwE (IllegalBehaviour (index x))
