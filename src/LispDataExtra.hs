{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LispDataExtra where

import LispData
import LispEnv

treatAsLispBool :: LispData -> EvalT Bool
treatAsLispBool (LispList _ [])    = return False
treatAsLispBool (LispBool _ False) = return False
treatAsLispBool _                  = return True
