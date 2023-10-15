{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Input
import NumericalDifferentiation
import Text.Layout.Table
import Text.Pretty.Simple
import Text.Printf

f :: Double -> Double
f x = exp $ 1.5 * x

f' :: Double -> Double
f' x = 1.5 * exp (1.5 * x)

f'' :: Double -> Double
f'' x = 2.25 * exp (1.5 * x)

prettyTable :: (Show a1, Show a2) => [(a1, a2)] -> String
prettyTable table =
    tableString $
        columnHeaderTableS [numCol, numCol] unicodeRoundS (titlesH ["x", "f(x)"]) $
            map (\(x, fx) -> rowG [show x, show fx]) table

prettyResultTable diffs =
    tableString
        $ columnHeaderTableS
            [numCol, numCol, numCol, numCol, numCol, numCol]
            unicodeRoundS
            ( titlesH
                [ "x_i"
                , "f(x_i)"
                , "f'(x_i)_num"
                , "|f'(x_i)_act - f'(x_i)_num|"
                , "f''(x_i)_num"
                , "|f''(x_i)_act - f''(x_i)_num|"
                ]
            )
        $ map
            ( \DifferentiationResult
                { xi
                , fxi
                , f'xiNum
                , difff'xiActAndNum
                , f''xiNum
                , difff''xiActAndNum
                } ->
                    rowG
                        [ show xi
                        , show fxi
                        , show f'xiNum
                        , show difff'xiActAndNum
                        , maybe "---" show f''xiNum
                        , maybe "---" show difff''xiActAndNum
                        ]
            )
            diffs

main :: IO ()
main = do
    putStrLn "Тема: Численное дифференцирование"
    putStrLn "Вариант: 15 mod 5 + 1 ≡ 1, f(x) = e^(1.5*x)"
    numOfPoints <- inputInt "Введите число точек (m+1): "
    startingPoint <- inputDouble "Введите начальную точку a: "
    diff <- inputDouble "Введите сдвиг h: "
    let table = genTable f numOfPoints diff startingPoint
    putStrLn "Таблица значений функции в точках вида x_i = a + i*h:"
    putStrLn $ prettyTable table
    let result = makeResult f' f'' table diff
    putStrLn "Итоговая таблица:"
    putStrLn $ prettyResultTable result
