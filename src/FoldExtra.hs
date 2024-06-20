module FoldExtra (foldM') where

import Control.Monad (foldM)

foldM' :: (Foldable f, Monad m) => b -> f a -> (b -> a -> m b) -> m b
foldM' zero xs f = foldM f zero xs
