module LispInterpreter (initEnv, interpretLisp) where

import Eval (eval)
import LispData (LispData(..))
import LispEnv (Eval, LispEnv)
import LispFunction (lispPredefFunctions)
import LispSyntax (lispPredefSyntaxes)

import Data.Functor ((<&>))

initEnv :: LispEnv
initEnv =
    lispPredefSyntaxes ++
    lispPredefFunctions

interpretLisp :: [LispData] -> Eval
interpretLisp ds = mapM eval ds <&> last
