{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispPredefSyntax where

import LispData
import LispError (LispError(..))
import LispInterpreter (evaluate)
import LispPredefUtil (LispFuncProg)

import Control.Lens (over)
import Data.Bifunctor (second)
import Data.Maybe (fromMaybe)
import Data.List (find)

lispPredefFuncsSyntax :: [LispData]
lispPredefFuncsSyntax =
    [ LispFunction (-1) "defun" lispDefun
    ]

lispDefun :: LispFuncProg
lispDefun ind _ args | length args < 3 =
    return $ Left (TooFewArguments ind 3)
lispDefun ind st args =
    sequence $
        expectIdentifier (head args) >>= verifyId >>=
            (\name ->
                expectList (args !! 1) >>= mapM expectIdentifier >>=
                    mapM verifyId >>=
                        (\args' ->
                            Right $
                                return
                                    ( over
                                        functions
                                        (++ [ LispFunction
                                                ind
                                                name
                                                (program args' (drop 2 args))
                                            ]
                                        )
                                        st
                                    , head args
                                    )
                        )
            )
    where
        program args' _ ind' _ args'' | length args' < length args'' =
            return $ Left (TooManyArguments ind' (length args'))
        program args' _ ind' _ args'' | length args' > length args'' =
            return $ Left (TooFewArguments ind' (length args'))
        program args' progs _ st' args'' = do
            res <- evaluate stWithArgs progs
            either (return . Left) (return . Right . second last) res
            where argVars = zipWith (LispVariable ind) args' args''
                  stWithArgs = over localVariables (++ argVars) st'

        search name filt store = find (filt name) (store st) >>=
            const (Just $ Left (IdentifierConfliction ind name))

        verifyId name =
            fromMaybe
                ( fromMaybe
                    ( fromMaybe
                        ( fromMaybe
                            ( fromMaybe
                                (Right name)
                                (search name varBindFilt _variables)
                            )
                            (search name varFilt _variables)
                        )
                        (search name varBindFilt _localVariables)
                    )
                    (search name varFilt _localVariables)
                )
                (search name funcFilt _functions)
