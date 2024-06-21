module LispFunction (lispPredefFunctions) where

import LispEnv (LispEnvData)
import LispMaths (lispPredefMathsFunctions)

lispPredefFunctions :: [(String, LispEnvData)]
lispPredefFunctions = lispPredefMathsFunctions
