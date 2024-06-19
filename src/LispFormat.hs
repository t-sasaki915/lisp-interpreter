{-# LANGUAGE LambdaCase #-}

module LispFormat (lispFormatString) where

import LispData
import LispError (LispError(..))
import ListExtra (replaceFirst)

import Control.Monad (foldM, (>=>))
import Control.Monad.Trans.Except (Except, throwE)
import Data.List (find)
import Data.Maybe (fromJust)
import Numeric (showHex, showOct)

data Command = ReplaceCommand Char String
             | ShowDataCommand Char (LispData -> Except LispError String)

commands :: [Command]
commands =
    [ ReplaceCommand  '~' "~"
    , ReplaceCommand  '%' "\n"
    , ShowDataCommand 'd' (expectNumber >=> (return . show))
    , ShowDataCommand 'x' (expectNumber >=> (return . (`showHex` "")))
    , ShowDataCommand 'o' (expectNumber >=> (return . (`showOct` "")))
    , ShowDataCommand 'a' (expectString >=> return)
    , ShowDataCommand 's' (return . show)
    ]

commandSymbol :: Command -> Char
commandSymbol (ReplaceCommand c _)  = c
commandSymbol (ShowDataCommand c _) = c

data CmdOpt = CmdOpt String (Maybe Int)

data Status = WaitingForTilde
            | ExpectingNumberOrCommand String String

extractCommands :: Int -> String -> Except LispError [(CmdOpt, Command)]
extractCommands n str = do
    (cmds, status) <- extractCommands' n str
    case status of
        WaitingForTilde -> return cmds
        _ -> throwE $ UnexpectedEndOfString n

extractCommands' :: Int -> String -> Except LispError ([(CmdOpt, Command)], Status)
extractCommands' n str = foldM
    (\case
        (cmds, WaitingForTilde) -> \case
            (_, '~') ->
                return (cmds, ExpectingNumberOrCommand "~" "")

            _ ->
                return (cmds, WaitingForTilde)
        
        (cmds, ExpectingNumberOrCommand it buffer) -> \case
            (_, c) | c `elem` map (head . show) ([0..9] :: [Int]) ->
                return
                    (cmds, ExpectingNumberOrCommand (it ++ [c]) (buffer ++ [c]))

            (cn, c) ->
                let it' = it ++ [c] in
                case find (\cmd -> commandSymbol cmd == c) commands of
                    Just (ReplaceCommand a b) | null buffer ->
                        return
                            ( cmds ++ [(CmdOpt it' Nothing, ReplaceCommand a b)]
                            , WaitingForTilde
                            )

                    Just (ReplaceCommand a _) ->
                        throwE $ IndentNotAllowed cn a
                    
                    Just (ShowDataCommand a b) | null buffer ->
                        return
                            ( cmds ++ [(CmdOpt it' (Just 0), ShowDataCommand a b)]
                            , WaitingForTilde
                            )

                    Just (ShowDataCommand a b) ->
                        return
                            ( cmds ++
                                [(CmdOpt it' (Just (read buffer)), ShowDataCommand a b)]
                            , WaitingForTilde
                            )

                    Nothing ->
                        throwE $ UnknownFormatCommand cn c
    )
    ([], WaitingForTilde) (zip [(n - length str)..] str)

replace :: Int -> String -> [(CmdOpt, Command)] -> [LispData] ->
           Except LispError (String, Int)
replace callIndex str cmds fills = foldM
    (\(str', refIndex) -> \case
        (CmdOpt lit _, ReplaceCommand _ alt) ->
            return (replaceFirst lit alt str', refIndex)

        (CmdOpt _ _, ShowDataCommand _ _) | refIndex >= length fills ->
            throwE $ TooFewArguments callIndex (refIndex + 3)

        (CmdOpt lit ni, ShowDataCommand _ f) -> do
            alt <- f (fills !! refIndex)
            let withIndent = addIndent alt (fromJust ni)
            return (replaceFirst lit withIndent str', refIndex + 1)
    )
    (str, 0) cmds
    where
        addIndent alt n | length alt >= n = alt
        addIndent alt n = replicate (n - length alt) ' ' ++ alt

lispFormatString :: Int -> LispData -> [LispData] -> Except LispError String
lispFormatString callIndex (LispString n str) fills = do
    cmds           <- extractCommands n str
    (formatted, _) <- replace callIndex str cmds fills
    return formatted

lispFormatString _ d _ =
    throwE $ TypeMismatch (lispDataIndex d) (show d) "String"
