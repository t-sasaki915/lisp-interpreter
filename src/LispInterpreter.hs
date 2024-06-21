module LispInterpreter (initEnv, interpretLisp) where

import Eval (eval)
import LispData (LispData(..))
import LispEnv (Eval, LispEnv)
import LispSyntax (lispPredefSyntaxes)

import Data.Functor ((<&>))

initEnv :: LispEnv
initEnv = lispPredefSyntaxes

interpretLisp :: [LispData] -> Eval
interpretLisp ds = mapM eval ds <&> last
