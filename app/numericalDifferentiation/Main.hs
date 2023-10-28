{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Input
import NumericalDifferentiation
import Text.Layout.Table

f :: Double -> Double
f x = exp $ 1.5 * x

f' :: Double -> Double
f' x = 1.5 * exp (1.5 * x)

f'' :: Double -> Double
f'' x = 2.25 * exp (1.5 * x)

prettyResultTable :: [DifferentiationResult] -> String
prettyResultTable diffs =
    tableString
        $ columnHeaderTableS
            [numCol, numCol, numCol, numCol, numCol, numCol, numCol, numCol]
            unicodeRoundS
            ( titlesH
                [ "x_i"
                , "f(x_i)"
                , "f'(x_i)_num"
                , "|f'(x_i)_act - f'(x_i)_num|"
                , "|f'(x_i)_act - f'(x_i)_num| / |f'(x_i)_act|"
                , "f''(x_i)_num"
                , "|f''(x_i)_act - f''(x_i)_num|"
                , "|f''(x_i)_act - f''(x_i)_num| / |f''(x_i)_act|"
                ]
            )
        $ map
            ( \DifferentiationResult
                { xi
                , fxi
                , f'xiNum
                , difff'xiActAndNum
                , relDifff'xiActAndNum
                , f''xiNum
                , difff''xiActAndNum
                , relDifff''xiActAndNum
                } ->
                    rowG
                        [ show xi
                        , show fxi
                        , show f'xiNum
                        , show difff'xiActAndNum
                        , show relDifff'xiActAndNum
                        , maybe "---" show f''xiNum
                        , maybe "---" show difff''xiActAndNum
                        , maybe "---" show relDifff''xiActAndNum
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
    --    putStrLn "Таблица значений функции в точках вида x_i = a + i*h:"
    --    putStrLn $ prettyTable table
    let result = makeResult f' f'' table diff
    putStrLn "Итоговая таблица:"
    putStrLn $ prettyResultTable result
