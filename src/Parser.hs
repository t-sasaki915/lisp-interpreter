module Parser (parse) where

import LispSystem (LispData(..))
import LispError (ParseError(..))

import Control.Monad.Trans.Except (Except, throwE)
import Data.Char (toUpper)
import Data.Functor ((<&>))
import Data.Ratio ((%))
import Text.Read (readMaybe)
import Text.Regex.Posix ((=~))

data Status = Start Int
            | CollectingElems Int [LispData]
            | ReadingStr Int String
            | ReadingSymb Int String
            | Ignoring Status

parse :: String -> Except ParseError [LispData]
parse src = recursive 0 []
    where
        recursive :: Int -> [LispData] -> Except ParseError [LispData]
        recursive i []  | i >= length src = throwE UnexpectedEOF
        recursive i lds | i >= length src = return lds
        recursive i lds = do
            (i', ld) <- parse' src i (Start 0)
            case ld of
                Just d  -> recursive (i' + 1) (lds ++ [d])
                Nothing -> recursive (i' + 1) lds

parse' :: String -> Int -> Status -> Except ParseError (Int, Maybe LispData)
parse' src i status | i >= length src =
    case status of
        (ReadingSymb nestDep buffer) -> do
            symb <- finaliseRead buffer <&> mkQuoteNest nestDep
            return (i - 1, Just symb)

        (Start 0) ->
            return (i, Nothing)

        (Ignoring (Start 0)) ->
            return (i, Nothing)

        _ ->
            throwE UnexpectedEOF

parse' src i (Start nestDep) =
    case src !! i of
        '(' ->
            parse' src (i + 1) (CollectingElems nestDep [])

        '\'' ->
            parse' src (i + 1) (Start (nestDep + 1))

        '"' ->
            parse' src (i + 1) (ReadingStr nestDep "")

        ';' ->
            parse' src (i + 1) (Ignoring (Start nestDep))

        c | c `elem` [' ', '\t', '\n'] ->
            parse' src (i + 1) (Start nestDep)

        ')' ->
            throwE (UnexpectedToken ')')
        
        c ->
            parse' src (i + 1) (ReadingSymb nestDep [c])

parse' src i (CollectingElems nestDep lst) =
    case src !! i of
        c | c `elem` [' ', '\t', '\n'] ->
            parse' src (i + 1) (CollectingElems nestDep lst)

        ';' ->
            parse' src (i + 1) (Ignoring (CollectingElems nestDep lst))

        ')' ->
            return (i, Just $ mkQuoteNest nestDep (LispList lst))

        _ -> do
            (i', dat) <- parse' src i (Start 0)
            case dat of
                Just d ->
                    parse' src (i' + 1) (CollectingElems nestDep (lst ++ [d]))

                Nothing ->
                    throwE UnexpectedEOF

parse' src i (ReadingStr nestDep buffer) =
    case src !! i of
        '"' ->
            return (i, Just $ mkQuoteNest nestDep (LispString buffer))

        c ->
            parse' src (i + 1) (ReadingStr nestDep (buffer ++ [c]))

parse' src i (ReadingSymb nestDep buffer) =
    case src !! i of
        c | c `elem` ['(', ')', '\'', '"', ';', ' ', '\t', '\n'] -> do
            symb <- finaliseRead buffer <&> mkQuoteNest nestDep
            return (i - 1, Just symb)

        c ->
            parse' src (i + 1) (ReadingSymb nestDep (buffer ++ [c]))

parse' src i (Ignoring restart) =
    case src !! i of
        '\n' ->
            parse' src (i + 1) restart

        _ ->
            parse' src (i + 1) (Ignoring restart)

mkQuoteNest :: Int -> LispData -> LispData
mkQuoteNest 0 d = d
mkQuoteNest n d = mkQuoteNest (n - 1) (LispQuote d)

finaliseRead :: String -> Except ParseError LispData
finaliseRead ('#' : '\\' : [x]) =
    return (LispCharacter x)

finaliseRead ('#' : '\\' : xs)  =
    case xs of
        "Backspace" -> return (LispCharacter '\b')
        "Tab"       -> return (LispCharacter '\t')
        "Page"      -> return (LispCharacter '\f')
        "Linefeed"  -> return (LispCharacter '\n')
        "Return"    -> return (LispCharacter '\r')
        _           -> throwE (UnknownChar xs)

finaliseRead buffer =
    case readMaybe buffer :: Maybe Integer of
        Just z ->
            return (LispInteger z)

        Nothing ->
            case readMaybe buffer :: Maybe Float of
                Just f ->
                    return (LispReal f)
    
                Nothing ->
                    let upperCase = map toUpper buffer in
                    case upperCase of
                        "T" ->
                            return (LispBool True)
                        "NIL" ->
                            return (LispBool False)

                        _ | upperCase =~ "[0-9]+\\/[0-9]+" == upperCase ->
                            case mapT read (break' (== '/') upperCase) of
                                (_, 0) -> throwE ZeroDivideCalculation'
                                (a, 1) -> return (LispInteger a)
                                (a, b) -> return (LispRational (a % b))

                        _ ->
                            return (LispSymbol upperCase)
    where
        mapT f (a, b) = (f a, f b)
        break' f xs = let (a, b) = break f xs in (a, tail b)
