module Parser (ParseError(..), parse) where

import LispData (LispData(..))
import LispError (ParseError(..))
import Util (break')

import Control.Monad.Trans.Except (Except, throwE)
import Data.Char (toUpper)
import Data.Functor ((<&>))
import Data.List (find)
import Text.Read (readMaybe)
import Text.Regex.Posix ((=~))

data Status = Start Int
            | ReadingSymb Int String
            | ReadingStr Int String
            | Ignoring


parse :: String -> Except ParseError [LispData]
parse src = parse' src 0 [] (Start 0) <&> snd


parse' :: String -> Int -> [LispData] -> Status ->
          Except ParseError (Int, [LispData])
parse' str i parsed status | i >= length str =
    case status of
        (Start 0) ->
            return (i - 1, reverse parsed)
        _ ->
            throwE UnexpectedEOF

parse' str i parsed (Start nestDep) =
    case str !! i of
        '(' -> do
            (i', lst) <- parse' str (i + 1) [] (Start 0)
            let newd = mkQuoteNest nestDep (LispList i' lst)
            parse' str (i' + 1) (newd : parsed) (Start 0)

        ')' ->
            return (i, reverse parsed)

        ';' ->
            parse' str (i + 1) parsed Ignoring

        '"' ->
            parse' str (i + 1) parsed (ReadingStr nestDep "")

        '\'' ->
            parse' str (i + 1) parsed (Start (nestDep + 1))

        c | c `elem` [' ', '\t', '\n'] ->
            parse' str (i + 1) parsed (Start nestDep)

        c ->
            parse' str (i + 1) parsed (ReadingSymb nestDep [c])

parse' str i parsed (ReadingSymb nestDep buf) =
    case str !! i of
        '(' -> do
            symb <- finaliseRead (i - 1) buf <&> mkQuoteNest nestDep
            (i', lst) <- parse' str (i + 1) [] (Start 0)
            parse' str (i' + 1) (LispList i' lst : symb : parsed) (Start 0)

        ')' -> do
            symb <- finaliseRead (i - 1) buf <&> mkQuoteNest nestDep
            return (i, reverse $ symb : parsed)

        ';' -> do
            symb <- finaliseRead (i - 1) buf <&> mkQuoteNest nestDep
            parse' str (i + 1) (symb : parsed) Ignoring

        '\'' -> do
            symb <- finaliseRead (i - 1) buf <&> mkQuoteNest nestDep
            parse' str (i + 1) (symb : parsed) (Start 1)

        c | c `elem` [' ', '\t', '\n'] -> do
            symb <- finaliseRead (i - 1) buf <&> mkQuoteNest nestDep
            parse' str (i + 1) (symb : parsed) (Start 0)

        c ->
            parse' str (i + 1) parsed (ReadingSymb nestDep (buf ++ [c]))

parse' str i parsed (ReadingStr nestDep buf) =
    case str !! i of
        '"' ->
            let lstr = mkQuoteNest nestDep (LispString i buf) in
            parse' str (i + 1) (lstr : parsed) (Start 0)

        c ->
            parse' str (i + 1) parsed (ReadingStr nestDep (buf ++ [c]))

parse' str i parsed Ignoring =
    case str !! i of
        '\n' ->
            parse' str (i + 1) parsed (Start 0)

        _ ->
            parse' str (i + 1) parsed Ignoring

mkQuoteNest :: Int -> LispData -> LispData
mkQuoteNest 0 d = d
mkQuoteNest n d = mkQuoteNest (n - 1) (LispQuote d)

finaliseRead :: Int -> String -> Except ParseError LispData
finaliseRead n buf =
    let buf' = map toUpper buf in
    case readMaybe buf :: Maybe Int of
        Just z -> return $ LispInteger n z
        Nothing ->
            case readMaybe buf :: Maybe Float of
                Just f -> return $ LispReal n f
                Nothing ->
                    case buf of
                        "#T" ->
                            return $ LispBool n True
                        "#t" ->
                            return $ LispBool n True
                        "#F" ->
                            return $ LispBool n False
                        "#f" ->
                            return $ LispBool n False
                        ('#' : '\\' : [x]) ->
                            return $ LispCharacter n x
                        ('#' : '\\' : xs) ->
                            analyseChar n (map toUpper xs)
                        _ | buf =~ "[0-9]+\\/[0-9]+" == buf ->
                            return $
                                uncurry (LispRational n)
                                    (mapT read (break' (== '/') buf))
                        _ ->
                            return $ LispSymbol n buf'
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
    [ ("BACKSPACE", '\b')
    , ("TAB"      , '\t')
    , ("PAGE"     , '\f')
    , ("LINEFEED" , '\n')
    , ("RETURN"   , '\r')
    ]
