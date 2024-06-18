module LispPredef (lispPredefinedFunctions) where

import LispData (LispData(..))
import LispPredefMathsLogic (lispPredefFuncsMathsLogic)
import LispPredefSyntax (lispPredefFuncsSyntax)

lispPredefinedFunctions :: [LispData]
lispPredefinedFunctions =
    lispPredefFuncsSyntax ++
    lispPredefFuncsMathsLogic
