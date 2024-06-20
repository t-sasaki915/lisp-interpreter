{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Util where

import Control.Monad (foldM)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except (Except, ExceptT, except, runExcept)

exceptT :: (Monad m) => Except a b -> ExceptT a m b
exceptT = except . runExcept

foldM' :: (Foldable f, Monad m) => b -> f a -> (b -> a -> m b) -> m b
foldM' zero xs f = foldM f zero xs

break' :: (a -> Bool) -> [a] -> ([a], [a])
break' f xs = let (a, b) = break f xs in (a, tail b)

lift2 :: (MonadTrans t, MonadTrans t', Monad m, Monad (t' m)) => m a -> t (t' m) a
lift2 = lift . lift
