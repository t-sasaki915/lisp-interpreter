{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispSyntax where

import Eval (eval, lexicalScope)
import LispError (RuntimeError(..))
import LispOperation
import LispSystem
import Util (getM, getOrElseM)

import Control.Monad.Trans.Except (throwE)

lispPredefSyntaxes :: [(String, LispData)]
lispPredefSyntaxes =
    [ ("IF"   ,  LispSyntax (-1) lispIF)
    , ("QUOTE",  LispSyntax (-1) lispQUOTE)
    , ("DEFINE", LispSyntax (-1) lispDEFINE)
    , ("LAMBDA", LispSyntax (-1) lispLAMBDA)
    ]

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
                _    <- putEnvData label (LispVariable ind (last expr))
                return (LispSymbol ind label)

            (LispList n []) ->
                throwE (TooFewArguments n "Definition" 1)

            (LispList _ binds) -> do
                label     <- treatAsLispSymbol (head binds)
                argLabels <- mapM treatAsLispSymbol (drop 1 binds)
                let prog :: Procedure
                    prog ind' args'
                        | length args' > length argLabels =
                            throwE (TooManyArguments ind' label (length argLabels))
                        | length args' < length argLabels =
                            throwE (TooFewArguments ind' label (length argLabels))
                        | otherwise =
                            lexicalScope
                                argLabels
                                args'
                                (tail args)

                _ <- putEnvData label (LispFunction ind prog)
                return (LispSymbol ind label)

            d ->
                throwE (incompatibleType d "SYMBOL")

lispLAMBDA :: Procedure
lispLAMBDA ind args
    | length args < 2 = throwE (TooFewArguments ind "LAMBDA" 2)
    | otherwise       = do
        bindList <- treatAsLispList (head args)
        bindings <- mapM treatAsLispSymbol bindList
        let
            program :: Procedure
            program ind' args'
                | length args' > length bindings =
                    throwE (TooManyArguments ind' "LAMBDA" (length bindings))
                | length args' < length bindings =
                    throwE (TooFewArguments ind' "LAMBDA" (length bindings))
                | otherwise =
                    lexicalScope bindings args' (tail args)

        return (LispFunction ind program)
