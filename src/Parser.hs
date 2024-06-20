module Parser (ParseError(..), parse) where

import ErrorTrace (Tracable(..))
import LispDataType (LispDataType(..))
import ListExtra (break')

import Control.Monad.Trans.Except (Except, throwE)
import Data.Functor ((<&>))
import Text.Read (readMaybe)
import Text.Regex.Posix ((=~))

data ParseError = UnexpectedEOF

instance Tracable ParseError where
    place UnexpectedEOF = 0
    title UnexpectedEOF = "Unexpected end of file"
    cause UnexpectedEOF = ""

data Status = Start
            | ReadingSymb String
            | ReadingStr String
            | Ignoring


parse :: String -> Except ParseError [LispDataType]
parse src = parse' src 0 [] Start <&> snd


parse' :: String -> Int -> [LispDataType] -> Status ->
          Except ParseError (Int, [LispDataType])
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

        c ->
            parse' str (i + 1) parsed (ReadingSymb [c])

parse' str i parsed (ReadingSymb buf) =
    case str !! i of
        '(' ->
            let symb = finaliseRead (i - 1) buf in do
            (i', lst) <- parse' str (i + 1) [] Start
            parse' str (i' + 1) (LispList i' lst : symb : parsed) Start

        ')' ->
            let symb = finaliseRead (i - 1) buf in
            return (i, reverse $ symb : parsed)

        ';' ->
            let symb = finaliseRead (i - 1) buf in
            parse' str (i + 1) (symb : parsed) Ignoring

        c | c `elem` [' ', '\t', '\n'] ->
            let symb = finaliseRead (i - 1) buf in
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


finaliseRead :: Int -> String -> LispDataType
finaliseRead n buffer =
    case readMaybe buffer :: Maybe Int of
        Just z -> LispInteger n z
        Nothing ->
            case readMaybe buffer :: Maybe Float of
                Just f -> LispReal n f
                Nothing ->
                    case buffer of
                        "#t" ->
                            LispBoolean n True
                        "#f" ->
                            LispBoolean n False
                        _ | buffer =~ "[0-9]+\\/[0-9]+" == buffer ->
                            uncurry (LispRational n)
                                    (mapT read (break' (== '/') buffer))
                        _ ->
                            LispSymbol n buffer
    where mapT f (a, b) = (f a, f b)
