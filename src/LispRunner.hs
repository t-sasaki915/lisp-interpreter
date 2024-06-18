module LispRunner (runLisp) where

import LispError (LispError(..))
import Syntax (Syntax(..))

runLisp :: String -> [Syntax] -> IO (Either LispError ())
runLisp src program = return $ Right ()
