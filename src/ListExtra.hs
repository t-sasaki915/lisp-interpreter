module ListExtra ((!?), replaceFirst, isPrimitive) where

import Data.List (elemIndices)

(!?) :: [a] -> Int -> Maybe a
(!?) xs i | i >= length xs = Nothing
          | otherwise      = Just (xs !! i)

isPrimitive :: Eq a => [a] -> Bool
isPrimitive xs = all (((< 2) . length) . (`elemIndices` xs)) xs

replaceFirst :: Eq a => [a] -> [a] -> [a] -> [a]
replaceFirst [] _ _ = []
replaceFirst matching alternative xs =
    if take (length matching) xs == matching then
        alternative ++ drop (length matching) xs

    else
        head xs : replaceFirst matching alternative (tail xs)
