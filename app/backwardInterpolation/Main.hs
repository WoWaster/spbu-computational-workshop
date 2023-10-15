module Main (main) where

import Data.Tuple
import Input
import Interpolation
import RootFinder
import Text.Layout.Table

f :: Double -> Double
f x = sin x + x ** 2 / 2.0

inputPower :: Int -> IO Int
inputPower m = do
    n <- inputInt "Введите степень интерполяционного многочлена n: "
    if n > (m - 1)
        then do
            putStrLn
                "Степень интерполяционного многочлена не может быть больше m!"
            inputPower m
        else return n

printTable :: (Show a1, Show a2) => [(a1, a2)] -> String
printTable table =
    tableString $
        columnHeaderTableS [numCol, numCol] unicodeRoundS (titlesH ["x", "f(x)"]) $
            map (\(x, fx) -> rowG [show x, show fx]) table

main :: IO ()
main = do
    putStrLn "Тема: Задача обратного интерполирования"
    putStrLn "Вариант: 16 mod 15 ≡ 1, f(x) = sin(x) + x^2/2"
    numOfInterpolationPoints <-
        inputInt "Введите число точек (m+1) в таблице: "
    leftBound <- inputDouble "Введите левую границу: "
    rightBound <- inputDouble "Введите правую границу: "
    epsilon <- inputDouble "Введите ε: "
    let table = genTable f leftBound rightBound numOfInterpolationPoints
    putStrLn "Таблица значений функции:"
    putStrLn $ printTable table
    fx <- inputDouble "Введите значении функции F: "
    powerOfPoly <- inputPower numOfInterpolationPoints
    -- First method
    let reversedTable = map swap table
    let reversedClosestPoints = sortPoints reversedTable fx
    let lagrangePolyFst = polynomialLagrange (powerOfPoly + 1) reversedClosestPoints
    let foundXFst = lagrangePolyFst fx
    putStrLn $ "Искомый X = " ++ show foundXFst
    putStrLn $ "|f(X) - F| = " ++ show (abs (f foundXFst - fx))
    -- Second method
    let middlePoint = (rightBound - leftBound) / 2
    let closestPoints = sortPoints table middlePoint
    let lagrangePolySnd x = polynomialLagrange (powerOfPoly + 1) closestPoints x - fx
    let rootIntervals =
            separateRoots
                leftBound
                rightBound
                lagrangePolySnd
                ((rightBound - leftBound) / 1e3)
    let roots =
            map
                (\(a, b) -> approximateByBisection a b lagrangePolySnd epsilon 0)
                rootIntervals
    putStrLn $ "Найдено корней: " ++ show (length roots)
    mapM_
        ( \res ->
            putStrLn $
                "Искомый X = "
                    ++ show (root res)
                    ++ "\n|f(X) - F| = "
                    ++ show (abs (f (root res) - fx))
        )
        roots
