{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}

module LispOperation where

import LispError 
import LispSystem

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.State.Strict (get, put)
import Data.Ratio ((%), numerator, denominator)

indexAndType :: LispData -> (Int, String)
indexAndType = \case
    (LispInteger n _)   -> (n, "INT")
    (LispReal n _)      -> (n, "REAL")
    (LispRational n _)  -> (n, "RATIONAL")
    (LispSymbol n _)    -> (n, "SYMBOL")
    (LispBool n _)      -> (n, "BOOL")
    (LispString n _)    -> (n, "STRING")
    (LispCharacter n _) -> (n, "CHAR")
    (LispList n _)      -> (n, "LIST")
    (LispPair n _)      -> (n, "PAIR")
    (LispQuote d)       -> mapSnd ("'" ++) (indexAndType d)
    (LispFunction _)    -> (-1, "FUNCTION")
    (LispSyntax _)      -> (-1, "SYNTAX")
    (LispVariable _)    -> (-1, "VARIABLE")
    where mapSnd f (a, b) = (a, f b)

toReal :: LispNumber -> Float
toReal (LispInteger' n)  = fromIntegral n
toReal (LispReal' r)     = r
toReal (LispRational' r) = fromRational r

incompatibleType :: LispData -> String -> RuntimeError
incompatibleType dat = uncurry IncompatibleType (indexAndType dat)

fromLispNumber :: Int -> LispNumber -> Execution LispData
fromLispNumber n (LispInteger' z)  = return (LispInteger n z)
fromLispNumber n (LispReal' r)     = return (LispReal n r)
fromLispNumber n (LispRational' r) = case (numerator r, denominator r) of
    (a, 1) -> return (LispInteger n a)
    (a, b) -> return (LispRational n (a % b))

treatAsLispBool :: LispData -> Execution Bool
treatAsLispBool (LispList _ [])    = return False
treatAsLispBool (LispBool _ False) = return False
treatAsLispBool _                  = return True

treatAsLispNumber :: LispData -> Execution LispNumber
treatAsLispNumber (LispInteger _ n)  = return (LispInteger' n)
treatAsLispNumber (LispRational _ r) = return (LispRational' r)
treatAsLispNumber (LispReal _ r)     = return (LispReal' r)
treatAsLispNumber d = throwE (incompatibleType d "NUMBER")

treatAsLispSymbol :: LispData -> Execution String
treatAsLispSymbol (LispSymbol _ s) = return s
treatAsLispSymbol d = throwE (incompatibleType d "SYMBOL")

unbindEnvData :: String -> Execution ()
unbindEnvData label = do
    env <- lift get
    let filtered = filter (\(l, _) -> l /= label) env
    lift $ put filtered

putEnvData :: String -> LispData -> Execution ()
putEnvData label expr = do
    _   <- unbindEnvData label
    env <- lift get
    lift $ put (env ++ [(label, expr)])
