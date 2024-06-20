module Parser (ParseError(..), parse) where

import LispData (LispData(..))
import LispError (ParseError(..))
import Util (break')

import Control.Monad.Trans.Except (Except, throwE)
import Data.Functor ((<&>))
import Data.List (find)
import Text.Read (readMaybe)
import Text.Regex.Posix ((=~))

data Status = Start
            | ReadingSymb String
            | ReadingStr String
            | Ignoring


parse :: String -> Except ParseError [LispData]
parse src = parse' src 0 [] Start <&> snd


parse' :: String -> Int -> [LispData] -> Status ->
          Except ParseError (Int, [LispData])
parse' str i parsed status | i >= length str =
    case status of
        Start ->
            return (i - 1, reverse parsed)
        _ ->
            throwE UnexpectedEOF

parse' str i parsed Start =
    case str !! i of
        '(' -> do
            (i', lst) <- parse' str (i + 1) [] Start
            parse' str (i' + 1) (LispList i' lst : parsed) Start

        ')' ->
            return (i, reverse parsed)

        ';' ->
            parse' str (i + 1) parsed Ignoring

        '"' ->
            parse' str (i + 1) parsed (ReadingStr "")

        c | c `elem` [' ', '\t', '\n'] ->
            parse' str (i + 1) parsed Start

        c ->
            parse' str (i + 1) parsed (ReadingSymb [c])

parse' str i parsed (ReadingSymb buf) =
    case str !! i of
        '(' -> do
            symb <- finaliseRead (i - 1) buf
            (i', lst) <- parse' str (i + 1) [] Start
            parse' str (i' + 1) (LispList i' lst : symb : parsed) Start

        ')' -> do
            symb <- finaliseRead (i - 1) buf
            return (i, reverse $ symb : parsed)

        ';' -> do
            symb <- finaliseRead (i - 1) buf
            parse' str (i + 1) (symb : parsed) Ignoring

        c | c `elem` [' ', '\t', '\n'] -> do
            symb <- finaliseRead (i - 1) buf
            parse' str (i + 1) (symb : parsed) Start

        c ->
            parse' str (i + 1) parsed (ReadingSymb (buf ++ [c]))

parse' str i parsed (ReadingStr buf) =
    case str !! i of
        '"' ->
            parse' str (i + 1) (LispString i buf : parsed) Start

        c ->
            parse' str (i + 1) parsed (ReadingStr (buf ++ [c]))

parse' str i parsed Ignoring =
    case str !! i of
        '\n' ->
            parse' str (i + 1) parsed Start

        _ ->
            parse' str (i + 1) parsed Ignoring


finaliseRead :: Int -> String -> Except ParseError LispData
finaliseRead n buffer =
    case readMaybe buffer :: Maybe Int of
        Just z -> return $ LispInteger n z
        Nothing ->
            case readMaybe buffer :: Maybe Float of
                Just f -> return $ LispReal n f
                Nothing ->
                    case buffer of
                        "#t" ->
                            return $ LispBool n True
                        "#f" ->
                            return $ LispBool n False
                        ('#' : '\\' : [x]) ->
                            return $ LispCharacter n x
                        ('#' : '\\' : xs) ->
                            analyseChar n xs
                        _ | buffer =~ "[0-9]+\\/[0-9]+" == buffer ->
                            return $
                                uncurry (LispRational n)
                                    (mapT read (break' (== '/') buffer))
                        _ ->
                            return $ LispSymbol n buffer
    where mapT f (a, b) = (f a, f b)

analyseChar :: Int -> String -> Except ParseError LispData
analyseChar n str =
    case find (\(l, _) -> str == l) specialChars of
        Just (_, c) ->
            return (LispCharacter n c)
                
        Nothing ->
            throwE (UnknownChar n str)

specialChars :: [(String, Char)]
specialChars =
    [ ("Backspace", '\b')
    , ("Tab"      , '\t')
    , ("Page"     , '\f')
    , ("Linefeed" , '\n')
    , ("Return"   , '\r')
    ]
