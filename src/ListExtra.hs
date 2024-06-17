module ListExtra ((!?), replaceFirst) where

(!?) :: [a] -> Int -> Maybe a
(!?) xs i | i >= length xs = Nothing
          | otherwise      = Just (xs !! i)

replaceFirst :: Eq a => [a] -> [a] -> [a] -> [a]
replaceFirst [] _ _ = []
replaceFirst matching alternative xs =
    if take (length matching) xs == matching then
        alternative ++ drop (length matching) xs

    else
        head xs : replaceFirst matching alternative (tail xs)
