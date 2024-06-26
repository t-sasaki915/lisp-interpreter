{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}

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
    , "LET"    ~> LispSyntax lispLET
    , "SET!"   ~> LispSyntax lispSETQ
    , "LAMBDA" ~> LispSyntax lispLAMBDA
    , "BEGIN"  ~> LispSyntax lispBEGIN
    ]

makeClosure :: [String] -> [LispData] -> Execution LispData
makeClosure binds progs = do
    (_, lexi) <- lift get <&> transformEnv
    let newLexi = lexi ++ map (~> LispVariableBind) binds
    return (LispClosure newLexi progs)

lispIF :: Procedure
lispIF args
    | length args > 3 = throwE (TooManyArguments "IF" 3)
    | length args < 2 = throwE (TooFewArguments "IF" 2)
    | otherwise       = do
        cond  <- eval (head args) >>= treatAsLispBool
        body1 <- getM args 1
        body2 <- getOrElseM args 2 (LispBool False)

        if cond then eval body1
                else eval body2

lispQUOTE :: Procedure
lispQUOTE args
    | length args > 1 = throwE (TooManyArguments "QUOTE" 1)
    | null args       = throwE (TooFewArguments "QUOTE" 1)
    | otherwise       = getM args 0

lispDEFINE :: Procedure
lispDEFINE args
    | length args < 2 = throwE (TooFewArguments "DEFINE" 2)
    | otherwise       =
        case head args of
            (LispSymbol label) -> do
                expr <- mapM eval (tail args)
                _    <- bindEnvDataGlobally label (LispVariable (last expr))
                return (LispSymbol label)

            (LispList []) ->
                throwE (TooFewArguments "Binding" 1)

            (LispList lst) -> do
                label    <- treatAsLispSymbol (head lst)
                bindings <- mapM treatAsLispSymbol (drop 1 lst)
                closure  <- makeClosure bindings (drop 1 args)
                _        <- bindEnvDataGlobally label (LispVariable closure)
                return (LispSymbol label)

            d ->
                throwE (IncompatibleType (dataType d) "SYMBOL")

lispLET :: Procedure
lispLET args
    | length args < 2 = throwE (TooFewArguments "LET" 2)
    | otherwise       = do
        bindList <- treatAsLispList (head args)
        bindings <-
                mapM
                    (\case
                        (LispSymbol label) ->
                            return (label ~> LispVariable (LispBool False))

                        (LispList lst) | length lst < 2 ->
                            throwE (TooFewArguments "Binding" 2)

                        (LispList lst) | length lst > 2 ->
                            throwE (TooManyArguments "Binding" 2)

                        (LispList lst) -> do
                            label <- treatAsLispSymbol (head lst)
                            value <- eval (lst !! 1)
                            return (label ~> LispVariable value)

                        d ->
                            throwE (IncompatibleType (dataType d) "SYMBOL")
                    )
                    bindList

        _      <- lexicalScope bindings
        values <- mapM eval (drop 1 args)
        _      <- finaliseLexicalScope
        return (last values)

lispSETQ :: Procedure
lispSETQ args
    | length args < 2 = throwE (TooFewArguments "SET!" 2)
    | otherwise       = do
        (globe, lexi) <- lift get <&> transformEnv
        label <- treatAsLispSymbol (head args)
        case lookup label lexi of
            Just (LispVariable _) -> rebind label bindEnvDataLexically

            Just LispVariableBind -> rebind label bindEnvDataLexically
            
            _ ->
                case lookup label globe of
                    Just (LispVariable _) -> rebind label bindEnvDataGlobally

                    Just LispVariableBind -> rebind label bindEnvDataGlobally

                    _ -> throwE (UndefinedVariable label)
    where
        rebind label binder = do
            value <- mapM eval (drop 1 args) <&> last
            _     <- binder label (LispVariable value)
            return value

lispLAMBDA :: Procedure
lispLAMBDA args
    | length args < 2 = throwE (TooFewArguments "LAMBDA" 2)
    | otherwise       = do
        bindList <- treatAsLispList (head args)
        bindings <- mapM treatAsLispSymbol bindList

        makeClosure bindings (drop 1 args)

lispBEGIN :: Procedure
lispBEGIN args
    | null args = return (LispBool False)
    | otherwise = mapM eval args <&> last
