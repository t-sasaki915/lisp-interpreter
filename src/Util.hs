module Util (getM, getOrElseM, (~>)) where

import Control.Monad.Trans.Except (ExceptT)

getM :: Monad m => [x] -> Int -> ExceptT a m x
getM xs i = return $ xs !! i

getOrElseM :: Monad m => [x] -> Int -> x -> ExceptT a m x
getOrElseM xs i def
    | i >= length xs = return def
    | otherwise      = return $ xs !! i

(~>) :: a -> b -> (a, b)
(~>) a b = (a, b)
