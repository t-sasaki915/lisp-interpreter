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
    [ "IF"           ~> LispSyntax lispIF
    , "QUOTE"        ~> LispSyntax lispQUOTE
    , "DEFUN"        ~> LispSyntax lispDEFUN
    , "DEFVAR"       ~> LispSyntax lispDEFVAR
    , "DEFPARAMETER" ~> LispSyntax lispDEFPARAMETER
    , "LET"          ~> LispSyntax lispLET
    , "SETQ"         ~> LispSyntax lispSETQ
    , "LAMBDA"       ~> LispSyntax lispLAMBDA
    , "PROGN"        ~> LispSyntax lispPROGN
    ]

makeClosure :: [String] -> [LispData] -> Execution LispData
makeClosure binds progs = do
    (globe, _) <- lift get <&> transformEnv
    mapM_ (unbindEnvDataLexically . fst) globe
    (_, lexi)  <- lift get <&> transformEnv
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

lispDEFUN :: Procedure
lispDEFUN args
    | length args < 3 = throwE (TooFewArguments "DEFUN" 3)
    | otherwise       = do
        label    <- treatAsLispSymbol (head args)
        bindList <- treatAsLispList (args !! 1)
        bindings <- mapM treatAsLispSymbol bindList
        closure  <- makeClosure bindings (drop 2 args)
        _        <- bindEnvDataGlobally label (LispVariable closure)
        return (LispSymbol label)

lispDEFVAR :: Procedure
lispDEFVAR args
    | null args        = throwE (TooFewArguments "DEFVAR" 1)
    | length args == 1 = do
        label <- treatAsLispSymbol (head args)
        _     <- bindEnvDataGlobally label LispVariableBind
        return (LispSymbol label)
    | otherwise        = do
        label <- treatAsLispSymbol (head args)
        value <- mapM eval (drop 1 args) <&> last
        _     <- bindEnvDataGlobally label (LispVariable value)
        return (LispSymbol label)

lispDEFPARAMETER :: Procedure
lispDEFPARAMETER args
    | length args < 2 = throwE (TooFewArguments "DEFPARAMETER" 2)
    | otherwise       = do
        label <- treatAsLispSymbol (head args)
        value <- mapM eval (drop 1 args) <&> last
        _     <- bindEnvDataGlobally label (LispVariable value)
        return (LispSymbol label)

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
    | length args < 2 = throwE (TooFewArguments "SETQ" 2)
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

lispPROGN :: Procedure
lispPROGN args
    | null args = return (LispBool False)
    | otherwise = mapM eval args <&> last
