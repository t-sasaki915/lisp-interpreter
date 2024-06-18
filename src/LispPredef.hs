module LispPredef (lispPredefinedFunctions) where

import LispData (LispData(..))
import LispPredefMathsLogic (lispPredefFuncsMathsLogic)

lispPredefinedFunctions :: [LispData]
lispPredefinedFunctions =
    lispPredefFuncsMathsLogic
