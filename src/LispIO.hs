{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispIO where

import LispError (RuntimeError(..))
import LispSystem
import Util ((~>))

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)

lispPredefIOFunctions :: [(String, LispEnvData)]
lispPredefIOFunctions =
    [ "DISPLAY" ~> LispFunction lispDISPLAY
    ]

lispDISPLAY :: Procedure
lispDISPLAY args
    | length args > 1 = throwE (TooManyArguments "DISPLAY" 1)
    | null args       = throwE (TooFewArguments "DISPLAY" 1)
    | otherwise       = do
        _ <- lift $ lift $ print (head args)
        return (LispBool False)
