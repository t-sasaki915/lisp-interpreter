module ListExtra
    ( (!?)
    , replaceFirst
    , isPrimitive
    , isEverythingSame
    ) where

import Data.List (nub)

(!?) :: [a] -> Int -> Maybe a
(!?) xs i | i >= length xs = Nothing
          | otherwise      = Just (xs !! i)

isPrimitive :: Eq a => [a] -> Bool
isPrimitive [] = True
isPrimitive xs = length (nub xs) == length xs

isEverythingSame :: Eq a => [a] -> Bool
isEverythingSame [] = True
isEverythingSame xs = length (nub xs) == 1

replaceFirst :: Eq a => [a] -> [a] -> [a] -> [a]
replaceFirst [] _ _ = []
replaceFirst matching alternative xs =
    if take (length matching) xs == matching then
        alternative ++ drop (length matching) xs

    else
        head xs : replaceFirst matching alternative (tail xs)
