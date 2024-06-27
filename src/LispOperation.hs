{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}

module LispOperation where

import LispError 
import LispSystem
import Util ((~>))

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.State.Strict (get, put)
import Data.Functor ((<&>))
import Data.Ratio ((%), numerator, denominator)

dataType :: LispData -> String
dataType = \case
    (LispInteger _)   -> "INT"
    (LispReal _)      -> "REAL"
    (LispRational _)  -> "RATIONAL"
    (LispSymbol _)    -> "SYMBOL"
    (LispBool _)      -> "BOOL"
    (LispString _)    -> "STRING"
    (LispCharacter _) -> "CHAR"
    (LispList _)      -> "LIST"
    (LispPair _)      -> "PAIR"
    (LispQuote d)     -> dataType d
    (LispClosure {})  -> "CLOSURE"

toReal :: LispNumber -> Float
toReal (LispInteger' n)  = fromIntegral n
toReal (LispReal' r)     = r
toReal (LispRational' r) = fromRational r

fromLispNumber :: LispNumber -> Execution LispData
fromLispNumber (LispInteger' z)  = return (LispInteger z)
fromLispNumber (LispReal' r)     = return (LispReal r)
fromLispNumber (LispRational' r) = case (numerator r, denominator r) of
    (a, 1) -> return (LispInteger a)
    (a, b) -> return (LispRational (a % b))

treatAsLispBool :: LispData -> Execution Bool
treatAsLispBool (LispList [])    = return False
treatAsLispBool (LispBool False) = return False
treatAsLispBool _                = return True

treatAsLispNumber :: LispData -> Execution LispNumber
treatAsLispNumber (LispInteger n)  = return (LispInteger' n)
treatAsLispNumber (LispRational r) = return (LispRational' r)
treatAsLispNumber (LispReal r)     = return (LispReal' r)
treatAsLispNumber d = throwE (IncompatibleType (dataType d) "NUMBER")

treatAsLispSymbol :: LispData -> Execution String
treatAsLispSymbol (LispSymbol s) = return s
treatAsLispSymbol d = throwE (IncompatibleType (dataType d) "SYMBOL")

treatAsLispList :: LispData -> Execution [LispData]
treatAsLispList (LispList l) = return l
treatAsLispList d = throwE (IncompatibleType (dataType d) "LIST")

treatAsLispInteger :: LispData -> Execution Integer
treatAsLispInteger (LispInteger n) = return n
treatAsLispInteger d = throwE (IncompatibleType (dataType d) "INT")

updateFunctionBinds :: [(String, LispEnvData)] -> Execution ()
updateFunctionBinds funcs = do
    env <- lift get
    lift $ put (LispEnv funcs (globalVariables env) (lexicalVariables env))

updateGlobalVarBinds :: [(String, Maybe LispData)] -> Execution ()
updateGlobalVarBinds vars = do
    env <- lift get
    lift $ put (LispEnv (functions env) vars (lexicalVariables env))

updateLexicalVarBinds :: [(String, Maybe LispData)] -> Execution ()
updateLexicalVarBinds vars = do
    env <- lift get
    lift $ put (LispEnv (functions env) (globalVariables env) vars)

unbindFunction :: String -> Execution ()
unbindFunction label = do
    funcs <- lift get <&> functions
    updateFunctionBinds (filter (\(l, _) -> l /= label) funcs)

bindFunction :: String -> LispEnvData -> Execution ()
bindFunction label func = do
    _     <- unbindFunction label
    funcs <- lift get <&> functions
    updateFunctionBinds (funcs ++ [label ~> func])

unbindGlobalVariable :: String -> Execution ()
unbindGlobalVariable label = do
    vars <- lift get <&> globalVariables
    updateGlobalVarBinds (filter (\(l, _) -> l /= label) vars)

bindGlobalVariable :: String -> Maybe LispData -> Execution ()
bindGlobalVariable label var = do
    _    <- unbindGlobalVariable label
    vars <- lift get <&> globalVariables
    updateGlobalVarBinds (vars ++ [label ~> var])

unbindLexicalVariable :: String -> Execution ()
unbindLexicalVariable label = do
    vars <- lift get <&> lexicalVariables
    updateLexicalVarBinds (filter (\(l, _) -> l /= label) vars)

bindLexicalVariable :: String -> Maybe LispData -> Execution ()
bindLexicalVariable label var = do
    _    <- unbindLexicalVariable label
    vars <- lift get <&> lexicalVariables
    updateLexicalVarBinds (vars ++ [label ~> var])

lookupFunction :: String -> Execution (Maybe LispEnvData)
lookupFunction label = do
    funcs <- lift get <&> functions
    return (lookup label funcs)

lookupGlobalVariable :: String -> Execution (Maybe (Maybe LispData))
lookupGlobalVariable label = do
    vars <- lift get <&> globalVariables
    return (lookup label vars)

lookupLexicalVariable :: String -> Execution (Maybe (Maybe LispData))
lookupLexicalVariable label = do
    vars <- lift get <&> lexicalVariables
    return (lookup label vars)

lexicalScope :: [(String, Maybe LispData)] -> Execution ()
lexicalScope = updateLexicalVarBinds

finaliseLexicalScope :: Execution ()
finaliseLexicalScope = updateLexicalVarBinds []
