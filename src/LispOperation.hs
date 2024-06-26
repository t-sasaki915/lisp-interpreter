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
    (LispClosure _ _) -> "CLOSURE"

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

transformEnv :: LispEnv -> ([(String, LispEnvData)], [(String, LispEnvData)])
transformEnv env = (global env, lexical env)

unbindEnvDataGlobally :: String -> Execution ()
unbindEnvDataGlobally label = do
    (globe, lexi) <- lift get <&> transformEnv
    lift $ put (LispEnv (filter (\(l, _) -> l /= label) globe) lexi)

bindEnvDataGlobally :: String -> LispEnvData -> Execution ()
bindEnvDataGlobally label envData = do
    _             <- unbindEnvDataGlobally label
    (globe, lexi) <- lift get <&> transformEnv
    lift $ put (LispEnv (globe ++ [label ~> envData]) lexi)

unbindEnvDataLexically :: String -> Execution ()
unbindEnvDataLexically label = do
    (globe, lexi) <- lift get <&> transformEnv
    lift $ put (LispEnv globe (filter (\(l, _) -> l /= label) lexi))

bindEnvDataLexically :: String -> LispEnvData -> Execution ()
bindEnvDataLexically label envData = do
    _             <- unbindEnvDataLexically label
    (globe, lexi) <- lift get <&> transformEnv
    lift $ put (LispEnv globe (lexi ++ [label ~> envData]))

lexicalScope :: [(String, LispEnvData)] -> Execution ()
lexicalScope binds = do
    (globe, lexi) <- lift get <&> transformEnv
    _             <- lift $ put (LispEnv globe (lexi ++ binds))
    return ()

finaliseLexicalScope :: Execution ()
finaliseLexicalScope = do
    (globe, _) <- lift get <&> transformEnv
    _          <- lift $ put (LispEnv globe [])
    return ()
