{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispSyntax where

import Eval (eval)
import LispError (RuntimeError(..))
import LispOperation
import LispSystem
import Util (getM, getOrElseM, (~>))

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.State.Strict (get)
import Data.Functor ((<&>))

lispPredefSyntaxes :: [(String, LispEnvData)]
lispPredefSyntaxes =
    [ "IF"     ~> LispSyntax lispIF
    , "QUOTE"  ~> LispSyntax lispQUOTE
    , "DEFINE" ~> LispSyntax lispDEFINE
    , "LAMBDA" ~> LispSyntax lispLAMBDA
    ]

makeClosure :: Int -> [String] -> [LispData] -> Execution LispData
makeClosure ind binds progs = do
    (_, lexi) <- lift get <&> transformEnv
    let newLexi = lexi ++ map (~> LispVariableBind) binds
    return (LispClosure ind newLexi progs)

lispIF :: Procedure
lispIF ind args
    | length args > 3 = throwE (TooManyArguments ind "IF" 3)
    | length args < 2 = throwE (TooFewArguments ind "IF" 2)
    | otherwise       = do
        cond  <- eval (head args) >>= treatAsLispBool
        body1 <- getM args 1
        body2 <- getOrElseM args 2 (LispBool ind False)

        if cond then eval body1
                else eval body2

lispQUOTE :: Procedure
lispQUOTE ind args
    | length args > 1 = throwE (TooManyArguments ind "QUOTE" 1)
    | null args       = throwE (TooFewArguments ind "QUOTE" 1)
    | otherwise       = getM args 0

lispDEFINE :: Procedure
lispDEFINE ind args
    | length args < 2 = throwE (TooFewArguments ind "DEFINE" 2)
    | otherwise       =
        case head args of
            (LispSymbol _ label) -> do
                expr <- mapM eval (tail args)
                _    <- bindEnvDataGlobally label (LispVariable (last expr))
                return (LispSymbol ind label)
            
            (LispList n []) ->
                throwE (TooFewArguments n "Binding" 1)

            (LispList _ lst) -> do
                label    <- treatAsLispSymbol (head lst)
                bindings <- mapM treatAsLispSymbol (drop 1 lst)
                closure  <- makeClosure ind bindings (drop 1 args)
                _        <- bindEnvDataGlobally label (LispVariable closure)
                return (LispSymbol ind label)

            d ->
                throwE (incompatibleType d "SYMBOL")

lispLAMBDA :: Procedure
lispLAMBDA ind args
    | length args < 2 = throwE (TooFewArguments ind "LAMBDA" 2)
    | otherwise       = do
        bindList <- treatAsLispList (head args)
        bindings <- mapM treatAsLispSymbol bindList

        makeClosure ind bindings (drop 1 args)
