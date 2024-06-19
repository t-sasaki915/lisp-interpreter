module LispPredef (lispPredefinedFunctions) where

import LispData (LispData(..))
import LispPredefIO (lispPredefFuncsIO)
import LispPredefMathsLogic (lispPredefFuncsMathsLogic)
import LispPredefSyntax (lispPredefFuncsSyntax)
import LispPredefSequence (lispPredefFuncsSequence)

lispPredefinedFunctions :: [LispData]
lispPredefinedFunctions =
    lispPredefFuncsSyntax ++
    lispPredefFuncsMathsLogic ++
    lispPredefFuncsSequence ++ 
    lispPredefFuncsIO
