{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}

module LispSyntax where

import Eval (eval, evalClosure)
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
    , "FUNCALL"      ~> LispSyntax lispFUNCALL
    ]

makeClosure :: [String] -> [LispData] -> Execution LispData
makeClosure binds progs = do
    globalVars  <- lift get <&> globalVariables
    mapM_ (unbindLexicalVariable . fst) globalVars
    lexicalVars <- lift get <&> lexicalVariables
    return (LispClosure lexicalVars binds progs)

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
        _        <- bindFunction label (LispUserFunction closure)
        return (LispSymbol label)

lispDEFVAR :: Procedure
lispDEFVAR args
    | null args        = throwE (TooFewArguments "DEFVAR" 1)
    | otherwise        = do
        label    <- treatAsLispSymbol (head args)
        maybeVar <- lookupGlobalVariable label
        case maybeVar of
            Just (Just _) ->
                return (LispSymbol label)

            _ | length args == 1 -> do
                _ <- bindGlobalVariable label Nothing
                return (LispSymbol label)
    
            _ -> do
                value <- mapM eval (drop 1 args) <&> last
                _     <- bindGlobalVariable label (Just value)
                return (LispSymbol label)

lispDEFPARAMETER :: Procedure
lispDEFPARAMETER args
    | length args < 2 = throwE (TooFewArguments "DEFPARAMETER" 2)
    | otherwise       = do
        label <- treatAsLispSymbol (head args)
        value <- mapM eval (drop 1 args) <&> last
        _     <- bindGlobalVariable label (Just value)
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
                            return (label ~> Just (LispBool False))

                        (LispList lst) | length lst < 2 ->
                            throwE (TooFewArguments "Binding" 2)

                        (LispList lst) | length lst > 2 ->
                            throwE (TooManyArguments "Binding" 2)

                        (LispList lst) -> do
                            label <- treatAsLispSymbol (head lst)
                            value <- eval (lst !! 1)
                            return (label ~> Just value)

                        d ->
                            throwE (IncompatibleType (dataType d) "SYMBOL")
                    )
                    bindList

        _     <- lexicalScope bindings
        value <- mapM eval (drop 1 args) <&> last
        _     <- finaliseLexicalScope
        return value

lispSETQ :: Procedure
lispSETQ args
    | length args < 2 = throwE (TooFewArguments "SETQ" 2)
    | otherwise       = do
        label           <- treatAsLispSymbol (head args)
        maybeLexicalVar <- lookupLexicalVariable label
        case maybeLexicalVar of
            Just _ -> do
                value <- mapM eval (drop 1 args) <&> last
                _     <- bindLexicalVariable label (Just value)
                return value

            Nothing -> do
                maybeGlobalVar <- lookupGlobalVariable label
                case maybeGlobalVar of
                    Just _ -> do
                        value <- mapM eval (drop 1 args) <&> last
                        _     <- bindGlobalVariable label (Just value)
                        return value

                    Nothing ->
                        throwE (UndefinedVariable label)

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

lispFUNCALL :: Procedure
lispFUNCALL args
    | null args = throwE (TooFewArguments "FUNCALL" 1)
    | otherwise = case head args of
        (LispClosure binds labels progs) ->
            evalClosure "This function" (LispClosure binds labels progs) (drop 1 args)

        (LispSymbol label) -> do
            closure <- eval (LispSymbol label)
            evalClosure label closure (drop 1 args)

        d ->
            throwE (IncompatibleType (dataType d) "FUNCTION")
