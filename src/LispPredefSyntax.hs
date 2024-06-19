{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}

module LispPredefSyntax where

import ExceptExtra (exceptT)
import LispData
import LispError (LispError(..))
import LispInterpreter (evaluateLisp)
import LispPredefUtil (LispFuncProg)
import ListExtra ((!?))

import Control.Lens (over)
import Control.Monad (foldM)
import Control.Monad.Trans.Except (throwE)
import Data.List (find)
import Data.Maybe (fromMaybe)

lispPredefFuncsSyntax :: [LispData]
lispPredefFuncsSyntax =
    [ LispFunction (-1) "defun" lispDefun
    , LispFunction (-1) "eval"  lispEval
    , LispFunction (-1) "if"    lispIf
    ]

lispDefun :: LispFuncProg
lispDefun ind _ args | length args < 3 =
    throwE $ TooFewArguments ind 3
lispDefun ind st args = do
    name   <- expectIdentifierT (head args)
    argLst <- expectListT (args !! 1) >>= gatherArgs
    st'    <- dropIntersect name st
    return ( over
                functions
                (++ [ LispFunction
                        ind
                        name
                        (program argLst (drop 2 args))
                    ]
                )
                st'
            , head args
            )
    where
        program args' _ ind' _ args'' | length args' < length args'' =
            throwE $ TooManyArguments ind' (length args')
        program args' _ ind' _ args'' | length args' > length args'' =
            throwE $ TooFewArguments ind' (length args')
        program args' progs _ st' args'' = do
            (st'', args''')  <- evaluateLisp st' args''
            (st''', progs')  <- exceptT $ replaceArgs args' progs st'' args'''
            (st'''', values) <- evaluateLisp st''' progs'
            return (st'''', last values)

        replaceArgs args' progs st' args'' = foldM
            (\(st'', progs') -> \case
                (LispIdentifier n l) ->
                    case find (\(a, _) -> a == l) (zip args' args'') of
                        Just (_, d) ->
                            return (st'', progs' ++ [d])

                        Nothing ->
                            return (st'', progs' ++ [LispIdentifier n l])

                (LispList n lst) -> do
                    (st''', lst') <- replaceArgs args' lst st'' args''
                    return (st''', progs' ++ [LispList n lst'])

                (LispLazyList n lst) -> do
                    (st''', lst') <- replaceArgs args' lst st'' args''
                    return (st''', progs' ++ [LispLazyList n lst'])

                d ->
                    return (st'', progs' ++ [d])

            ) (st', []) progs

        dropIntersect name st' = return $
            over functions (filter (not . funcFilt name)) st'

        gatherArgs = foldM
            (\known -> \case
                (LispIdentifier n l) | l `elem` known ->
                    throwE $ IdentifierConfliction n l
                (LispIdentifier _ l) ->
                    return (known ++ [l])
                d -> 
                    throwE $ TypeMismatch (lispDataIndex d) (show d) "Identifier"
            ) []

lispEval :: LispFuncProg
lispEval ind _ args | length args > 1 =
    throwE $ TooManyArguments ind 1
lispEval ind _ args | null args =
    throwE $ TooFewArguments ind 1
lispEval _ st args = do
    (st', args') <- evaluateLisp st [head args]
    let whatToEval = case last args' of
                        (LispLazyList n lst) -> LispList n lst
                        d -> d
    (st'', vars) <- evaluateLisp st' [whatToEval]
    return (st'', last vars)

lispIf :: LispFuncProg
lispIf ind _ args | length args > 3 =
    throwE $ TooManyArguments ind 3
lispIf ind _ args | length args < 2 =
    throwE $ TooFewArguments ind 2
lispIf ind st args = do
    (st', vars) <- evaluateLisp st [head args]
    cond        <- expectBoolT (last vars)

    let whatToEval = if cond then args !! 1
                     else fromMaybe (LispBool ind False) (args !? 2)

    (st'', vars') <- evaluateLisp st' [whatToEval]
    return (st'', last vars')
