module Interpolation (genTable, takeClosestPoints, polynomialNewton, polynomialLagrange) where

import Data.List (sortOn)

genTable :: (Double -> Double) -> Double -> Double -> Int -> [(Double, Double)]
genTable f leftBound rightBound nOfPoints =
    [ (x, f x)
    | let h = (rightBound - leftBound) / fromIntegral (nOfPoints - 1)
    , j <- [0 .. (nOfPoints - 1)]
    , let x = leftBound + fromIntegral j * h
    ]

takeClosestPoints :: [(Double, Double)] -> Double -> Int -> [Double]
takeClosestPoints table point nOfPoints = map fst $ take (nOfPoints + 1) $ sortOn (\(x, _) -> abs $ x - point) table

-- Next three functions written by @KubEf with small modifications by me
splitDiff :: (Fractional a, Eq a) => [a] -> (a -> a) -> a
splitDiff xs f = sum [f xj / product [xj - xi | xi <- xs, xi /= xj] | xj <- xs]

polynomialNewton :: (Fractional b, Eq b) => (b -> b) -> Int -> [b] -> b -> b
polynomialNewton f n xs x = helper n
  where
    helper 1 = f $ head xs
    helper m =
        (product [x - xi | xi <- take (m - 1) xs]) * splitDiff (take m xs) f
            + helper (m - 1)

polynomialLagrange :: (Eq a, Fractional a) => (a -> a) -> Int -> [a] -> a -> a
polynomialLagrange f n xs x = sum [f xi * linearComb xi | xi <- take n xs]
  where
    linearComb xi = product [(x - xj) / (xi - xj) | xj <- take n xs, xi /= xj]
