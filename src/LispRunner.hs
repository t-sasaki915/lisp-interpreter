module LispRunner (runLisp) where

import LispData (LispState(..))
import LispError (LispError(..))
import LispInterpreter (translate, evaluate)
import Syntax (Syntax(..))

runLisp :: [Syntax] -> IO (Either LispError ())
runLisp program = do
    case translate initState program of
        Right (state, program') -> do
            res <- evaluate state program'
            case res of
                Right (_, []) ->
                    return $ Right ()

                Right (_, [v]) ->
                    print v >>
                        return (Right ())

                Right (_, v : _) ->
                    print v >>
                        return (Right ())

                Left err ->
                    return $ Left err

        Left err ->
            return $ Left err

    where initState = LispState [] [] []
