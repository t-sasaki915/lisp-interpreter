module ExceptExtra (exceptT) where

import Control.Monad.Trans.Except (Except, ExceptT, except, runExcept)

exceptT :: Monad m => Except a b -> ExceptT a m b
exceptT = except . runExcept
