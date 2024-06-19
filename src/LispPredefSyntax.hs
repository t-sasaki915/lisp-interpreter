{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}

module LispPredefSyntax where

import LispData
import LispError (LispError(..))
import LispInterpreter (evaluateLisp)
import LispPredefUtil (LispFuncProg)
import ListExtra ((!?))

import Control.Lens (over)
import Control.Monad.Trans.Except (throwE)
import Data.Maybe (fromMaybe)
import Data.List (find)

lispPredefFuncsSyntax :: [LispData]
lispPredefFuncsSyntax =
    [ LispFunction (-1) "defun" lispDefun
    , LispFunction (-1) "if"    lispIf
    ]

lispDefun :: LispFuncProg
lispDefun ind _ args | length args < 3 =
    throwE $ TooFewArguments ind 3
lispDefun ind st args = do
    name    <- expectIdentifierT (head args)
    _       <- verifyId name
    argLst' <- expectListT (args !! 1)
    argLst  <- mapM expectIdentifierT argLst'
    _       <- mapM verifyId argLst
    return ( over
                functions
                (++ [ LispFunction
                        ind
                        name
                        (program argLst (drop 2 args))
                    ]
                )
                st
            , head args
            )
    where
        program args' _ ind' _ args'' | length args' < length args'' =
            throwE $ TooManyArguments ind' (length args')
        program args' _ ind' _ args'' | length args' > length args'' =
            throwE $ TooFewArguments ind' (length args')
        program args' progs _ st' args'' = do
            (st'', vars) <- evaluateLisp stWithArgs progs
            return (st'', last vars)
            where argVars = zipWith (LispVariable ind) args' args''
                  stWithArgs = over localVariables (++ argVars) st'

        search name filt store = find (filt name) (store st) >>=
            const (Just $ throwE (IdentifierConfliction ind name))

        verifyId name =
            fromMaybe
                ( fromMaybe
                    ( fromMaybe
                        (return ())
                        (search name varFilt _variables)
                    )
                    (search name varFilt _localVariables)
                )
                (search name funcFilt _functions)

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
