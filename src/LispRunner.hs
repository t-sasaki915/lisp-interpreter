module LispRunner (runLisp) where

import Lisp
import SyntaxAnalyser (LispSyntax(..))

runLisp :: [LispSyntax] -> IO (Either LispError ())
runLisp program = do
    result <- parseSyntaxes initState program
    case result of
        Right (_, lispData) -> do
            print $ last lispData
            return $ Right ()

        Left err ->
            return $ Left err
    where
        initState =
            LispState
                lispPredefinedSyntaxes
                lispPredefinedFunctions
                []
