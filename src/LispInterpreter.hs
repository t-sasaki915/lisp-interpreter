module LispInterpreter (initEnv, interpretLisp) where

import Eval (eval)
import LispIO (lispPredefIOFunctions)
import LispMaths (lispPredefMathsFunctions)
import LispSyntax (lispPredefSyntaxes)
import LispSystem

import Data.Functor ((<&>))

initEnv :: LispEnv
initEnv = LispEnv initGlobe []
    where
        initGlobe =
            lispPredefSyntaxes ++
            lispPredefMathsFunctions ++
            lispPredefIOFunctions

interpretLisp :: [LispData] -> Execution LispData
interpretLisp ds = mapM eval ds <&> last
