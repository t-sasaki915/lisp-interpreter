module ErrorTrace (Tracable(..), traceError) where

class Tracable a where
    place :: a -> (String, Int)
    title :: a -> String
    cause :: a -> String

traceError :: (Tracable a) => a -> String
traceError err =
    "(Line " ++ line ++ ", Index " ++ index ++ ") " ++ title err ++
        case cause err of
            "" -> "."
            c  -> ": " ++ c
    where
        line = show $ length (lines (take (ind err + 1) (src err)))
        index = show $ subtract 1 (length (last (lines (take (ind err + 1) (src err)))))
        src = fst . place
        ind = snd . place
