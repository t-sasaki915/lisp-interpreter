module LispRunner (runLisp) where

import LispData (LispState(..))
import LispError (LispError(..))
import LispInterpreter (translate, evaluate)
import LispPredef (lispPredefinedFunctions)
import Syntax (Syntax(..))

runLisp :: [Syntax] -> IO (Either LispError ())
runLisp program = do
    case translate initState program of
        Right (state, program') -> do
            res <- evaluate state program'
            case res of
                Right (_, v) ->
                    print (last v) >>
                        return (Right ())

                Left err ->
                    return $ Left err

        Left err ->
            return $ Left err

    where initState = LispState lispPredefinedFunctions [] []
