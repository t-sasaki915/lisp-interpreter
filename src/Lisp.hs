{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Lisp
    ( LispError(..)
    , LispData(..)
    , LispState(..)
    , lispPredefinedFunctions
    ) where

import ListExtra ((!?), replaceFirst)

import Control.Lens
import Control.Monad (foldM)
import Data.List (find)
import Text.Read (readMaybe)

data LispError = UndefinedIdentifier String
               | InvalidUsageOfFunction String
               | FormatError String String
               | InvalidToken String
               | InvalidProgramFormat
               | EmptyExecList
               deriving (Eq, Show)

data LispData = LispString String
              | LispNumber Int
              | LispBool Bool
              | LispList [LispData]
              | LispVariable String LispData
              | LispVariableBind String
              | LispFunction String ([LispData] -> LispState ->
                             IO (Either LispError (LispState, LispData)))

instance Eq LispData where
    (==) (LispString s1) (LispString s2)             = s1 == s2
    (==) (LispNumber n1) (LispNumber n2)             = n1 == n2
    (==) (LispBool b1) (LispBool b2)                 = b1 == b2
    (==) (LispList l1) (LispList l2)                 = l1 == l2
    (==) (LispVariable n1 d1) (LispVariable n2 d2)   = n1 == n2 && d1 == d2
    (==) (LispVariableBind n1) (LispVariableBind n2) = n1 == n2
    (==) (LispFunction n1 _) (LispFunction n2 _)     = n1 == n2
    (==) _ _                                         = False

instance Show LispData where
    show (LispString a)       = "\"" ++ a ++ "\""
    show (LispNumber a)       = show a
    show (LispBool True)      = "t"
    show (LispBool False)     = "nil"
    show (LispList as)        = "'(" ++ unwords (map show as) ++ ")"
    show (LispVariable a _)   = a
    show (LispVariableBind a) = a
    show (LispFunction a _)   = a

data LispState = LispState
    { _functions      :: [LispData]
    , _variables      :: [LispData]
    }
    deriving (Eq, Show)

makeLenses ''LispState

lispPredefinedFunctions :: [LispData]
lispPredefinedFunctions =
    [ LispFunction "+"       lispAddition
    , LispFunction "-"       lispSubtraction
    , LispFunction "*"       lispMultiplication
    , LispFunction "/"       lispDivision
    , LispFunction "max"     lispMax
    , LispFunction "min"     lispMin
    , LispFunction "abs"     lispAbs
    , LispFunction "listp"   lispListp
    , LispFunction "atom"    lispAtom
    , LispFunction "null"    lispNull
    , LispFunction "eq"      lispEq
    , LispFunction "equal"   lispEqual
    , LispFunction "minusp"  lispMinusp
    , LispFunction "plusp"   lispPlusp
    , LispFunction "numberp" lispNumberp
    , LispFunction "zerop"   lispZerop
    , LispFunction "and"     lispAnd
    , LispFunction "or"      lispOr
    , LispFunction "not"     lispNot
    , LispFunction "car"     lispCar
    , LispFunction "cdr"     lispCdr
    , LispFunction "cons"    lispCons
    , LispFunction "append"  lispAppend
    , LispFunction "member"  lispMember
    , LispFunction "reverse" lispReverse
    , LispFunction "format"  lispFormat
    , LispFunction "eval"    lispEval
    , LispFunction "defun"   lispDefun
    ]

invalidFuncUsage :: String -> LispState -> IO (Either LispError (LispState, LispData))
invalidFuncUsage name _ = return (Left $ InvalidUsageOfFunction name)

purely :: LispData -> LispState -> IO (Either LispError (LispState, LispData))
purely d state = return (Right (state, d))

type LispFuncProg = [LispData] -> LispState ->
                    IO (Either LispError (LispState, LispData))

lispAddition :: LispFuncProg
lispAddition [LispNumber n1, LispNumber n2] = purely $ LispNumber (n1 + n2)
lispAddition _                              = invalidFuncUsage "+"

lispSubtraction :: LispFuncProg
lispSubtraction [LispNumber n1, LispNumber n2] = purely $ LispNumber (n1 - n2)
lispSubtraction _                              = invalidFuncUsage "-"

lispMultiplication :: LispFuncProg
lispMultiplication [LispNumber n1, LispNumber n2] = purely $ LispNumber (n1 * n2)
lispMultiplication _                              = invalidFuncUsage "*"

lispDivision :: LispFuncProg
lispDivision [LispNumber n1, LispNumber n2] = purely $ LispNumber (n1 `div` n2)
lispDivision _                              = invalidFuncUsage "/"

lispMax :: LispFuncProg
lispMax xs s = return (mapM num xs >>= (\a -> Right (s, LispNumber (maximum a))))
    where num = \case (LispNumber n) -> Right n
                      _              -> Left $ InvalidUsageOfFunction "max"

lispMin :: LispFuncProg
lispMin xs s = return (mapM num xs >>= (\a -> Right (s, LispNumber (minimum a))))
    where num = \case (LispNumber n) -> Right n
                      _              -> Left $ InvalidUsageOfFunction "min"

lispAbs :: LispFuncProg
lispAbs [LispNumber n] = purely $ LispNumber (abs n)
lispAbs _              = invalidFuncUsage "abs"

lispListp :: LispFuncProg
lispListp [LispList _] = purely $ LispBool True
lispListp [_]          = purely $ LispBool False
lispListp _            = invalidFuncUsage "listp"

lispAtom :: LispFuncProg
lispAtom [LispString _] = purely $ LispBool True
lispAtom [LispNumber _] = purely $ LispBool True
lispAtom [LispBool _]   = purely $ LispBool True
lispAtom [_]            = purely $ LispBool False
lispAtom _              = invalidFuncUsage "atom"

lispNull :: LispFuncProg
lispNull [LispList lst] = purely $ LispBool (null lst)
lispNull _              = invalidFuncUsage "null"

lispEq :: LispFuncProg
lispEq [a, b] = purely $ LispBool (a == b)
lispEq _      = invalidFuncUsage "eq"

lispEqual :: LispFuncProg
lispEqual [a, b] = purely $ LispBool (a == b)
lispEqual _      = invalidFuncUsage "equal"

lispMinusp :: LispFuncProg
lispMinusp [LispNumber n] = purely $ LispBool (n < 0)
lispMinusp _              = invalidFuncUsage "minusp"

lispPlusp :: LispFuncProg
lispPlusp [LispNumber n] = purely $ LispBool (n > 0)
lispPlusp _              = invalidFuncUsage "plusp"

lispNumberp :: LispFuncProg
lispNumberp [LispNumber _] = purely $ LispBool True
lispNumberp [_]            = purely $ LispBool False
lispNumberp _              = invalidFuncUsage "numberp"

lispZerop :: LispFuncProg
lispZerop [LispNumber n] = purely $ LispBool (n == 0)
lispZerop _              = invalidFuncUsage "zerop"

lispAnd :: LispFuncProg
lispAnd [LispBool b1, LispBool b2] = purely $ LispBool (b1 && b2)
lispAnd _                          = invalidFuncUsage "and"

lispOr :: LispFuncProg
lispOr  [LispBool b1, LispBool b2] = purely $ LispBool (b1 || b2)
lispOr _                           = invalidFuncUsage "or"

lispNot :: LispFuncProg
lispNot [LispBool b] = purely $ LispBool (not b)
lispNot _            = invalidFuncUsage "not"

lispCar :: LispFuncProg
lispCar [LispList xs] = purely $ head xs
lispCar _             = invalidFuncUsage "car"

lispCdr :: LispFuncProg
lispCdr [LispList xs] = purely $ LispList (drop 1 xs)
lispCdr _             = invalidFuncUsage "cdr"

lispCons :: LispFuncProg
lispCons [x, LispList xs] = purely $ LispList (x : xs)
lispCons _                = invalidFuncUsage "cons"

lispAppend :: LispFuncProg
lispAppend [LispList xs, LispList ys] = purely $ LispList (xs ++ ys)
lispAppend _                          = invalidFuncUsage "append"

lispMember :: LispFuncProg
lispMember [x, LispList xs] = purely $ LispList (dropWhile (x /=) xs)
lispMember _                = invalidFuncUsage "member"

lispReverse :: LispFuncProg
lispReverse [LispList xs] = purely $ LispList (reverse xs)
lispReverse _             = invalidFuncUsage "reverse"

data FormatCommand = ShowTilde String
                   | ShowNewLine String
                   | ShowNumberDecimal String Int
                   | ShowString String Int
                   | ShowData String Int

data FCmdSyntax = FCmdSyntax Char (String -> FormatCommand)
                | FCmdSyntaxIndentAllowed Char (String -> Int -> FormatCommand)


fCmdSyntaxes :: [FCmdSyntax]
fCmdSyntaxes =
    [ FCmdSyntax              '~' ShowTilde
    , FCmdSyntax              '%' ShowNewLine
    , FCmdSyntaxIndentAllowed 'd' ShowNumberDecimal
    , FCmdSyntaxIndentAllowed 'a' ShowString
    , FCmdSyntaxIndentAllowed 's' ShowData
    ]

fCmdLetter :: FCmdSyntax -> Char
fCmdLetter (FCmdSyntax c _)              = c
fCmdLetter (FCmdSyntaxIndentAllowed c _) = c

getFCmdSyntax :: Char -> Maybe FCmdSyntax
getFCmdSyntax c = find findFilter fCmdSyntaxes
    where findFilter = \case (FCmdSyntax c' _)              -> c' == c
                             (FCmdSyntaxIndentAllowed c' _) -> c' == c

data FormatCommandExtractStep = WaitForTilde
                              | ExpectNumberOrCommand String String

extractFormatCommands :: String -> Either LispError [FormatCommand]
extractFormatCommands str = fst $ foldl
    (\state charac ->
        case state of
            (Left _, _) -> state
            (Right cmds, WaitForTilde) ->
                case charac of
                    '~' ->
                        (Right cmds, ExpectNumberOrCommand "~" "")

                    _ ->
                        state

            (Right cmds, ExpectNumberOrCommand raw buffer) ->
                case charac of
                    c | c `elem` map (head . show) ([0..9] :: [Int]) ->
                        ( Right cmds
                        , ExpectNumberOrCommand (raw ++ [c]) (buffer ++ [c])
                        )

                    c | c `elem` map fCmdLetter fCmdSyntaxes ->
                        case getFCmdSyntax c of
                            Just (FCmdSyntax _ f) | null buffer ->
                                (Right $ cmds ++ [f (raw ++ [c])], WaitForTilde)

                            Just (FCmdSyntaxIndentAllowed _ f) | null buffer ->
                                (Right $ cmds ++ [f (raw ++ [c]) 0], WaitForTilde)

                            Just (FCmdSyntax _ _) ->
                                ( Left $ FormatError "Indent is not allowed" [c]
                                , WaitForTilde
                                )

                            Just (FCmdSyntaxIndentAllowed _ f) ->
                                case readMaybe buffer of
                                    Just i ->
                                        ( Right $ cmds ++ [f (raw ++ [c]) i]
                                        , WaitForTilde
                                        )

                                    Nothing ->
                                        ( Left $ FormatError "Not a number" buffer
                                        , WaitForTilde
                                        )

                            Nothing ->
                                ( Left $ FormatError "Command not found" [c]
                                , WaitForTilde
                                )

                    c ->
                        ( Left $ FormatError "Unrecognisable character" [c]
                        , WaitForTilde
                        )
    )
    (Right [], WaitForTilde)
    str

makeString :: String -> [LispData] -> [FormatCommand] -> Either LispError String
makeString str args cmds = fst $ foldl
    (\state factor ->
        case state of
            (Left _, _) -> state
            (Right txt, argIndex) ->
                case factor of
                    (ShowTilde cmd) ->
                        (Right $ replaceFirst cmd "~" txt, argIndex)

                    (ShowNewLine cmd) ->
                        (Right $ replaceFirst cmd "\n" txt, argIndex)

                    (ShowNumberDecimal cmd ind) ->
                        case args !? argIndex of
                            Just (LispNumber n) ->
                                ( Right $ replaceFirst cmd (addIndent (show n) ind) txt
                                , argIndex + 1
                                )

                            Just d ->
                                ( Left $ FormatError "Not a number" (show d)
                                , 0
                                )

                            Nothing ->
                                tooFewArgHalt argIndex

                    (ShowString cmd ind) ->
                        case args !? argIndex of
                            Just (LispString s) ->
                                ( Right $ replaceFirst cmd (addIndent s ind) txt
                                , argIndex + 1
                                )

                            Just d ->
                                ( Left $ FormatError "Not a string" (show d)
                                , 0
                                )

                            Nothing ->
                                tooFewArgHalt argIndex

                    (ShowData cmd ind) ->
                        case args !? argIndex of
                            Just d ->
                                ( Right $ replaceFirst cmd (addIndent (show d) ind) txt
                                , argIndex + 1
                                )

                            Nothing ->
                                tooFewArgHalt argIndex
    )
    (Right str, 0)
    cmds
    where
        tooFewArgHalt i =
            ( Left $ FormatError "Insufficient what to format" (show (i - 1)), 0)
        addIndent d ind | length d >= ind = d
        addIndent d ind = replicate (ind - length d) ' ' ++ d

lispFormat :: LispFuncProg
lispFormat ((LispBool True) : (LispString str) : xs) s =
    sequence $
        extractFormatCommands str >>=
            makeString str xs >>=
                (\f -> Right $ putStr f >> return (s, LispBool False))
lispFormat ((LispBool False) : (LispString str) : xs) s =
    sequence $
        extractFormatCommands str >>=
            makeString str xs >>=
                (\f -> Right $ return (s, LispString f))
lispFormat _ s = invalidFuncUsage "format" s

lispEval :: LispFuncProg
lispEval [LispList [LispFunction _ f]]        = f []
lispEval [LispList (LispFunction _ f : args)] = f args
lispEval _                                    = invalidFuncUsage "eval"

lispDefun :: LispFuncProg
lispDefun (LispVariableBind name : LispList args : progs) s =
    return $
        Right
            ( over functions (++ [LispFunction name program]) s
            , LispBool False
            )
    where
        program :: LispFuncProg
        program args' s' | length args /= length args' =
            invalidFuncUsage name s'
        program args' s' =
            let
                varsEither =
                    mapM bindName args >>=
                        (\a -> mapM (Right . uncurry LispVariable) (zip a args'))
            in
            case varsEither of
                Right vars ->
                    foldM
                        (\state d ->
                            case state of
                                (Left _) -> return state
                                (Right (s'', _)) ->
                                    case d of
                                        (LispList [LispFunction _ f]) ->
                                            f [] s''

                                        _ ->
                                            return $ Right (s'', d)
                        )
                        (Right (over variables (++ vars) s', LispBool False))
                        progs

                Left err ->
                    return $ Left err

        bindName = \case (LispVariableBind a) -> Right a
                         _ -> Left (InvalidUsageOfFunction "defun")

lispDefun _ s = invalidFuncUsage "defun" s
