module Main (main) where

import Input
import Interpolation
import Text.Pretty.Simple
import Text.Printf

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

main :: IO ()
main = do
    putStrLn "Тема: Задача алгебраического интерполирования"
    putStrLn "Вариант: 16 mod 15 ≡ 1, f(x) = sin(x) + x^2/2"
    numOfInterpolationPoints <-
        inputInt "Введите число точек интерполирования (m+1): "
    leftBound <- inputDouble "Введите левую границу: "
    rightBound <- inputDouble "Введите правую границу: "
    let table = genTable f leftBound rightBound numOfInterpolationPoints
    putStrLn "Таблица значений функции:"
    pPrint table
    x <- inputDouble "Введите точку интерполяции x: "
    let fx = f x
    powerOfPoly <- inputPower numOfInterpolationPoints
    let closestPoints = sortPoints table x
    pPrint closestPoints
    let lagrangePoly =
            polynomialLagrange (powerOfPoly + 1) closestPoints
    let lagrangeX = lagrangePoly x
    printf "\nP^L_%d(%f) = %f\n" powerOfPoly x lagrangeX
    printf "|f(%f) - P^L_%d(%f)| = %0f\n" x powerOfPoly x $ abs (fx - lagrangeX)
    let newtonPoly = polynomialNewton (powerOfPoly + 1) closestPoints
    let newtonX = newtonPoly x
    printf "\nP^N_%d(%f) = %f\n" powerOfPoly x newtonX
    printf "|f(%f) - P^N_%d(%f)| = %0f\n" x powerOfPoly x $ abs (fx - newtonX)
