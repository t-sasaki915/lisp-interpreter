{-# LANGUAGE LambdaCase #-}

module Lisp
    ( LispError(..)
    , LispData(..)
    , LispState(..)
    , lispPredefinedFunctions
    ) where

import ListExtra ((!?), replaceFirst)

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
              | LispRawList [LispData]
              | LispVariable String LispData
              | LispVariableBind String
              | LispFunction String ([LispData] -> LispState ->
                             IO (Either LispError (LispState, LispData)))

instance Eq LispData where
    (==) (LispString s1) (LispString s2)             = s1 == s2
    (==) (LispNumber n1) (LispNumber n2)             = n1 == n2
    (==) (LispBool b1) (LispBool b2)                 = b1 == b2
    (==) (LispList l1) (LispList l2)                 = l1 == l2
    (==) (LispRawList l1) (LispRawList l2)           = l1 == l2
    (==) (LispVariable n1 d1) (LispVariable n2 d2)   = n1 == n2 && d1 == d2
    (==) (LispVariableBind n1) (LispVariableBind n2) = n1 == n2
    (==) (LispFunction n1 _) (LispFunction n2 _)     = n1 == n2
    (==) _ _                                         = False

instance Show LispData where
    show (LispString a)       = "\"" ++ a ++ "\""
    show (LispNumber a)       = show a
    show (LispBool True)      = "t"
    show (LispBool False)     = "nil"
    show (LispList as)        = "(" ++ unwords (map show as) ++ ")"
    show (LispRawList as)     = "'(" ++ unwords (map show as) ++ ")"
    show (LispVariable a _)   = a
    show (LispVariableBind a) = a
    show (LispFunction a _)   = a

data LispState = LispState
    { _functions :: [LispData]
    , _variables :: [LispData]
    }
    deriving (Eq, Show)

lispPredefinedFunctions :: [LispData]
lispPredefinedFunctions =
    [ lispAddition
    , lispSubtraction
    , lispMultiplication
    , lispDivision
    , lispMax
    , lispMin
    , lispAbs
    , lispListp
    , lispAtom
    , lispNull
    , lispEq
    , lispEqual
    , lispMinusp
    , lispPlusp
    , lispNumberp
    , lispZerop
    , lispAnd
    , lispOr
    , lispNot
    , lispCar
    , lispCdr
    , lispCons
    , lispAppend
    , lispMember
    , lispReverse
    , lispFormat
    ]

invalidFuncUsage :: String -> LispState -> IO (Either LispError (LispState, LispData))
invalidFuncUsage name _ = return (Left $ InvalidUsageOfFunction name)

purely :: LispData -> LispState -> IO (Either LispError (LispState, LispData))
purely d state = return (Right (state, d))

lispAddition :: LispData
lispAddition = LispFunction "+" program
    where
        program [LispNumber n1, LispNumber n2] = purely $ LispNumber (n1 + n2)
        program _                              = invalidFuncUsage "+"

lispSubtraction :: LispData
lispSubtraction = LispFunction "-" program
    where
        program [LispNumber n1, LispNumber n2] = purely $ LispNumber (n1 - n2)
        program _                              = invalidFuncUsage "-"

lispMultiplication :: LispData
lispMultiplication = LispFunction "*" program
    where
        program [LispNumber n1, LispNumber n2] = purely $ LispNumber (n1 * n2)
        program _                              = invalidFuncUsage "*"

lispDivision :: LispData
lispDivision = LispFunction "/" program
    where
        program [LispNumber n1, LispNumber n2] = purely $ LispNumber (n1 `div` n2)
        program _                              = invalidFuncUsage "/"

lispMax :: LispData
lispMax = LispFunction "max" program
    where
        program xs s = return (mapM num xs >>= (\a -> Right (s, LispNumber (maximum a))))
        num = \case (LispNumber n) -> Right n
                    _              -> Left $ InvalidUsageOfFunction "max"

lispMin :: LispData
lispMin = LispFunction "min" program
    where
        program xs s = return (mapM num xs >>= (\a -> Right (s, LispNumber (minimum a))))
        num = \case (LispNumber n) -> Right n
                    _              -> Left $ InvalidUsageOfFunction "min"

lispAbs :: LispData
lispAbs = LispFunction "abs" program
    where
        program [LispNumber n] = purely $ LispNumber (abs n)
        program _              = invalidFuncUsage "abs"

lispListp :: LispData
lispListp = LispFunction "listp" program
    where
        program [LispList _]    = purely $ LispBool True
        program [LispRawList _] = purely $ LispBool True
        program [_]             = purely $ LispBool False
        program _               = invalidFuncUsage "listp"

lispAtom :: LispData
lispAtom = LispFunction "atom" program
    where
        program [LispString _] = purely $ LispBool True
        program [LispNumber _] = purely $ LispBool True
        program [LispBool _]   = purely $ LispBool True
        program [_]            = purely $ LispBool False
        program _              = invalidFuncUsage "atom"

lispNull :: LispData
lispNull = LispFunction "null" program
    where
        program [LispList lst]    = purely $ LispBool (null lst)
        program [LispRawList lst] = purely $ LispBool (null lst)
        program _                 = invalidFuncUsage "null"

lispEq :: LispData
lispEq = LispFunction "eq" program
    where
        program [a, b] = purely $ LispBool (a == b)
        program _      = invalidFuncUsage "eq"

lispEqual :: LispData
lispEqual = LispFunction "equal" program
    where
        program [a, b] = purely $ LispBool (a == b)
        program _      = invalidFuncUsage "equal"

lispMinusp :: LispData
lispMinusp = LispFunction "minusp" program
    where
        program [LispNumber n] = purely $ LispBool (n < 0)
        program _              = invalidFuncUsage "minusp"

lispPlusp :: LispData
lispPlusp = LispFunction "plusp" program
    where
        program [LispNumber n] = purely $ LispBool (n > 0)
        program _              = invalidFuncUsage "plusp"

lispNumberp :: LispData
lispNumberp = LispFunction "numberp" program
    where
        program [LispNumber _] = purely $ LispBool True
        program [_]            = purely $ LispBool False
        program _              = invalidFuncUsage "numberp"

lispZerop :: LispData
lispZerop = LispFunction "zerop" program
    where
        program [LispNumber n] = purely $ LispBool (n == 0)
        program _              = invalidFuncUsage "zerop"

lispAnd :: LispData
lispAnd = LispFunction "and" program
    where
        program [LispBool b1, LispBool b2] = purely $ LispBool (b1 && b2)
        program _                          = invalidFuncUsage "and"

lispOr :: LispData
lispOr = LispFunction "or" program
    where
        program [LispBool b1, LispBool b2] = purely $ LispBool (b1 || b2)
        program _                          = invalidFuncUsage "or"

lispNot :: LispData
lispNot = LispFunction "not" program
    where
        program [LispBool b] = purely $ LispBool (not b)
        program _            = invalidFuncUsage "not"

lispCar :: LispData
lispCar = LispFunction "car" program
    where
        program [LispList xs]    = purely $ head xs
        program [LispRawList xs] = purely $ head xs
        program _                = invalidFuncUsage "car"

lispCdr :: LispData
lispCdr = LispFunction "cdr" program
    where
        program [LispList xs]    = purely $ LispList (drop 1 xs)
        program [LispRawList xs] = purely $ LispRawList (drop 1 xs)
        program _                = invalidFuncUsage "cdr"

lispCons :: LispData
lispCons = LispFunction "cons" program
    where
        program [x, LispList xs]    = purely $ LispList (x : xs)
        program [x, LispRawList xs] = purely $ LispRawList (x : xs)
        program _                   = invalidFuncUsage "cons"

lispAppend :: LispData
lispAppend = LispFunction "append" program
    where
        program [LispList xs, LispList ys]       = purely $ LispList (xs ++ ys)
        program [LispRawList xs, LispRawList ys] = purely $ LispRawList (xs ++ ys)
        program _                                = invalidFuncUsage "append"

lispMember :: LispData
lispMember = LispFunction "member" program
    where
        program [x, LispList xs]    = purely $ LispList (dropWhile (x /=) xs)
        program [x, LispRawList xs] = purely $ LispRawList (dropWhile (x /=) xs)
        program _                   = invalidFuncUsage "member"

lispReverse :: LispData
lispReverse = LispFunction "reverse" program
    where
        program [LispList xs]    = purely $ LispList (reverse xs)
        program [LispRawList xs] = purely $ LispRawList (reverse xs)
        program _                = invalidFuncUsage "reverse"

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
                        (Right cmds, ExpectNumberOrCommand (raw ++ [c]) (buffer ++ [c]))

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
                                        (Right $ cmds ++ [f (raw ++ [c]) i], WaitForTilde)

                                    Nothing ->
                                        ( Left $ FormatError "Not a number" buffer
                                        , WaitForTilde
                                        )

                            Nothing ->
                                ( Left $ FormatError "Command not found" [c]
                                , WaitForTilde
                                )

                    c ->
                        (Left $ FormatError "Unrecognisable character" [c], WaitForTilde)
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

lispFormat :: LispData
lispFormat = LispFunction "format" program
    where
        program ((LispBool True) : (LispString str) : xs) s =
            sequence $
                extractFormatCommands str >>=
                    makeString str xs >>=
                        (\f -> Right $ putStr f >> return (s, LispBool False))
        program ((LispBool False) : (LispString str) : xs) s =
            sequence $
                extractFormatCommands str >>=
                    makeString str xs >>=
                        (\f -> Right $ return (s, LispString f))
        program _ s = invalidFuncUsage "format" s
