{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}

module LispPredef where

import LispData
import LispError (LispError(..))
import LispInterpreter (evaluate)
import ListExtra (isPrimitive)

lispPredefinedFunctions :: [LispData]
lispPredefinedFunctions =
    [ lispSimpleFunction "*"  lispMultiplication
    , lispSimpleFunction "+"  lispAddition
    , lispSimpleFunction "-"  lispSubtraction
    , lispSimpleFunction "/"  lispDivision
    , lispSimpleFunction "/=" lispNotEqual
    , lispSimpleFunction "1+" lispOnePlus
    , lispSimpleFunction "1-" lispOneMinus
    ]

type LispFuncProg = Int -> LispState -> [LispData] ->
                    IO (Either LispError (LispState, LispData))

lispSimpleFunction :: String -> LispFuncProg -> LispData
lispSimpleFunction name prog = LispFunction (-1) name prog'
    where
        prog' index state args = do
            res <- evaluate state args
            either (return . Left) (uncurry (prog index)) res

lispMultiplication :: LispFuncProg
lispMultiplication ind st args =
    sequence $
        mapM expectNumber args >>=
            (\case
                [] -> Right $ return (st, LispNumber ind 1)
                xs -> Right $ return (st, LispNumber ind (product xs))
            )

lispAddition :: LispFuncProg
lispAddition ind st args =
    sequence $
        mapM expectNumber args >>=
            (\case
                [] -> Right $ return (st, LispNumber ind 0)
                xs -> Right $ return (st, LispNumber ind (sum xs))
            )

lispSubtraction :: LispFuncProg
lispSubtraction ind st args =
    sequence $
        mapM expectNumber args >>=
            (\case
                []  -> Left $ TooFewArguments ind 1
                [x] -> Right $ return (st, LispNumber ind (negate x))
                xs  -> Right $ return (st, LispNumber ind (foldl1 (-) xs))
            )

lispDivision :: LispFuncProg
lispDivision ind st args =
    sequence $
        mapM expectNumber args >>=
            (\case
                []  -> Left $ TooFewArguments ind 1
                [x] -> Right $ return (st, LispNumber ind (1 `div` x))
                xs  -> Right $ return (st, LispNumber ind (foldl1 div xs))
            )

lispNotEqual :: LispFuncProg
lispNotEqual ind st args =
    sequence $
        mapM expectNumber args >>=
            (\case
                []  -> Left $ TooFewArguments ind 1
                xs  -> Right $ return (st, LispBool ind (isPrimitive xs))
            )

lispOnePlus :: LispFuncProg
lispOnePlus ind _ args | length args > 1 =
    return $ Left (TooManyArguments ind 1)
lispOnePlus ind _ args | null args =
    return $ Left (TooFewArguments ind 1)
lispOnePlus ind st args =
    sequence $
        expectNumber (head args) >>=
            (\x -> Right $ return (st, LispNumber ind (x + 1)))

lispOneMinus :: LispFuncProg
lispOneMinus ind _ args | length args > 1 =
    return $ Left (TooManyArguments ind 1)
lispOneMinus ind _ args | null args =
    return $ Left (TooFewArguments ind 1)
lispOneMinus ind st args =
    sequence $
        expectNumber (head args) >>=
            (\x -> Right $ return (st, LispNumber ind (x - 1)))
