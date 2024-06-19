{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}

module LispPredefSequence where

import LispData
import LispError (LispError(..))
import LispPredefUtil (lispSimpleFunction, LispFuncProg)

import Control.Monad.Trans.Except (throwE)

lispPredefFuncsSequence :: [LispData]
lispPredefFuncsSequence =
    [ lispSimpleFunction "append" lispAppend
    ]

lispAppend :: LispFuncProg
lispAppend ind st args =
    mapM expectLazyListT args >>=
        (\case
            [] -> return (st, LispBool ind False)
            xs -> return (st, LispLazyList ind (concat xs))
        )
