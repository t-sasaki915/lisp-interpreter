module LispPredefUtil (LispFuncProg, lispSimpleFunction) where

import LispData (LispState, LispData(LispFunction))
import LispError (LispError(..))
import LispInterpreter (evaluateLisp)

import Control.Monad.Trans.Except (ExceptT)

type LispFuncProg = Int -> LispState -> [LispData] ->
                    ExceptT LispError IO (LispState, LispData)

lispSimpleFunction :: String -> LispFuncProg -> LispData
lispSimpleFunction name prog = LispFunction (-1) name prog'
    where prog' index state args =
            evaluateLisp state args >>= uncurry (prog index)
