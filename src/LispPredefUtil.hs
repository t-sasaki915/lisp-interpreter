module LispPredefUtil (LispFuncProg, lispSimpleFunction) where

import LispData
import LispError (LispError(..))
import LispInterpreter (evaluate)

type LispFuncProg = Int -> LispState -> [LispData] ->
                    IO (Either LispError (LispState, LispData))

lispSimpleFunction :: String -> LispFuncProg -> LispData
lispSimpleFunction name prog = LispFunction (-1) name prog'
    where
        prog' index state args = do
            res <- evaluate state args
            either (return . Left) (uncurry (prog index)) res
