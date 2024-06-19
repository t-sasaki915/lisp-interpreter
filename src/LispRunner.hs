module LispRunner (runLisp) where

import ExceptExtra (exceptT)
import LispData (LispState(..))
import LispError (LispError(..))
import LispInterpreter (translateSyntax, evaluateLisp)
import LispPredef (lispPredefinedFunctions)
import Syntax (Syntax(..))

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)

runLisp :: [Syntax] -> ExceptT LispError IO ()
runLisp program = do
    (st, prog)  <- exceptT $ translateSyntax initState program
    (_, values) <- evaluateLisp st prog
    lift $ print (last values)
    where initState = LispState lispPredefinedFunctions [] []
