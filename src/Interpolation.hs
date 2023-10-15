module Interpolation (genTable, sortPoints, polynomialNewton, polynomialLagrange) where

import Data.List (sortOn)

genTable :: (Double -> Double) -> Double -> Double -> Int -> [(Double, Double)]
genTable f leftBound rightBound nOfPoints =
    [ (x, f x)
    | let h = (rightBound - leftBound) / fromIntegral (nOfPoints - 1)
    , j <- [0 .. (nOfPoints - 1)]
    , let x = leftBound + fromIntegral j * h
    ]

sortPoints :: [(Double, Double)] -> Double -> [(Double, Double)]
sortPoints table point = sortOn (\(x, _) -> abs $ x - point) table

-- Next three functions written by @KubEf with small modifications by me
splitDiff :: (Fractional a, Eq a) => [(a, a)] -> a
splitDiff table =
    sum
        [fxj / product [xj - xi | (xi, _) <- table, xi /= xj] | (xj, fxj) <- table]

polynomialNewton :: (Fractional b, Eq b) => Int -> [(b, b)] -> b -> b
polynomialNewton n table x = helper n
  where
    helper 1 = (snd . head) table
    helper m =
        (product [x - xi | (xi, _) <- take (m - 1) table]) * splitDiff (take m table)
            + helper (m - 1)

polynomialLagrange :: (Eq a, Fractional a) => Int -> [(a, a)] -> a -> a
polynomialLagrange n table x = sum [fxi * linearComb xi | (xi, fxi) <- take n table]
  where
    linearComb xi = product [(x - xj) / (xi - xj) | (xj, _) <- take n table, xi /= xj]
