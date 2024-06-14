module LispRunner (LispError(..), runLisp) where

import ErrorTrace (Tracable(..))
import SyntaxAnalyser (LispSyntax(..))

data LispError = UndefinedFunction String Int String
               | InvalidArgumentNumber String Int Int
               deriving (Eq, Show)

instance Tracable LispError where
    place (UndefinedFunction a b _)     = (a, b)
    place (InvalidArgumentNumber a b _) = (a, b)

    title (UndefinedFunction {})        = "Undefined function"
    title (InvalidArgumentNumber {})    = "Invalid argument number"

    cause (UndefinedFunction _ _ a)     = a
    cause (InvalidArgumentNumber _ _ a) = show a

runLisp :: String -> [LispSyntax] -> IO (Either LispError ())
runLisp src program = pure (Right ())
