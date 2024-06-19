module LispFormat (lispFormatString) where

import LispData
import LispError (LispError(..))

import Control.Monad.Trans.Except (Except)

lispFormatString :: String -> [LispData] -> Except LispError String
lispFormatString str fills = return str
