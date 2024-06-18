module ErrorTrace (Tracable(..), traceError) where

class Tracable a where
    place :: a -> Int
    title :: a -> String
    cause :: a -> String

traceError :: (Tracable a) => a -> String -> String
traceError err src =
    "(Line " ++ line ++ ", Index " ++ index ++ ") " ++ title err ++
        case cause err of
            "" -> "."
            c  -> ": " ++ c
    where
        line = show $ length (lines (take (place err + 1) src))
        index = show $ subtract 1 (length (last (lines (take (place err + 1) src))))
