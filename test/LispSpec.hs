module LispSpec (lispProcedureTest) where

import LispInterpreter (initEnv)
import LispSystem

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Strict (runStateT)
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit (Test(TestCase), assertEqual)

lispProcedureTest :: Procedure -> [LispData] -> Execution LispData -> Test
lispProcedureTest f args e =
    let
        result =
            fst $ unsafePerformIO $ runStateT (runExceptT (f args)) initEnv
        e' =
            fst $ unsafePerformIO $ runStateT (runExceptT e) initEnv
    in
    TestCase $ assertEqual "" result e'
