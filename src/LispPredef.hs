module LispPredef (lispPredefinedFunctions) where

import LispData (LispData(..))
import LispPredefIO (lispPredefFuncsIO)
import LispPredefMathsLogic (lispPredefFuncsMathsLogic)
import LispPredefSyntax (lispPredefFuncsSyntax)

lispPredefinedFunctions :: [LispData]
lispPredefinedFunctions =
    lispPredefFuncsSyntax ++
    lispPredefFuncsMathsLogic ++
    lispPredefFuncsIO
