{-# LANGUAGE OverloadedLists #-}

module Interpolation (genTable, takeClosestPoints, makeLagrangePoly, makeNewtonPoly, eval1) where

import Data.List (sortOn)

import Data.Poly
import Debug.Trace

genTable :: (Double -> Double) -> Double -> Double -> Int -> [(Double, Double)]
genTable f leftBound rightBound nOfPoints =
    [ (x, f x)
    | let h = (rightBound - leftBound) / fromIntegral (nOfPoints - 1)
    , j <- [0 .. (nOfPoints - 1)]
    , let x = leftBound + fromIntegral j * h
    ]

takeClosestPoints :: [(Double, Double)] -> Double -> Int -> [(Double, Double)]
takeClosestPoints table point nOfPoints = take (nOfPoints + 1) $ sortOn (\(x, _) -> abs $ x - point) table

xMinus :: Double -> UPoly Double
xMinus y = [-y, 1] :: UPoly Double

scalarPoly :: Double -> UPoly Double
scalarPoly x = [x] :: UPoly Double

makeLagrangeBasisPoly ::
    [(Double, Double)] -> Double -> (Double, [UPoly Double])
makeLagrangeBasisPoly table xk = (omega'AtXk, omegaWithoutXkRoot)
  where
    omegaWithoutXkRoot =
        map
            (\(xj, _) -> if xj /= xk then xMinus xj else scalarPoly 1)
            table
    omega'AtXk = foldr ((*) . (`eval` xk)) 1 omegaWithoutXkRoot

makeLagrangePoly :: [(Double, Double)] -> [(Double, [UPoly Double])]
makeLagrangePoly table =
    map
        ( \(xj, fxj) ->
            let (coeff, basisPoly) = makeLagrangeBasisPoly table xj
             in (fxj / coeff, basisPoly)
        )
        table

eval1 :: [(Double, [UPoly Double])] -> Double -> Double
eval1 poly x =
    foldr
        (\(coeff, poly1) acc -> acc + coeff * foldr ((*) . (`eval` x)) 1 poly1)
        0
        poly

getDividedDifference :: [(Double, Double)] -> Double
getDividedDifference table = foldr (\(xj, fxj) acc -> acc + fxj / prod xj) 0 table
  where
    prod xj = foldr (\(yj, _) acc -> if xj == yj then acc else acc * (xj - yj)) 1.0 table

makeNewtonPoly :: [(Double, Double)] -> UPoly Double
makeNewtonPoly table = helper table (scalarPoly 0)
  where
    helper [(_, fxj)] acc = scalarPoly fxj + acc
    helper t@(_ : xs) acc =
        helper
            xs
            ( acc
                + scalarPoly (getDividedDifference t)
                    * foldr (\(yj, _) poly -> poly * xMinus yj) (scalarPoly 1) xs
            )
    helper [] _ = error "В таблице не может быть 0 значений!"
