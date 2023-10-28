{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module RootFinder (
    separateRoots,
    approximateByBisection,
    approximateByNewton,
    approximateByModifiedNewton,
    approximateBySecant,
    MyResult,
    root,
    delta,
    discrepancy,
    counter,
) where

-- This file is made by @KubEf with small modifications by me
data MyResult = MyResult
    { root :: Double
    , delta :: Double
    , discrepancy :: Double
    , counter :: Int
    }

findNewtonStartingPoints ::
    (Ord p, Ord a, Fractional a, Num p) => (p -> a) -> (p -> a) -> p -> p -> p -> p
findNewtonStartingPoints f f'' leftBound rightBound precision =
    helper leftBound
  where
    helper leftBound'
        | leftBound' > rightBound =
            error "Не могу найти начальную точку для метода Ньютона."
        | f leftBound' * f'' leftBound' > 0.0 =
            leftBound'
        | otherwise =
            helper (leftBound' + precision)

separateRoots ::
    (Ord t, Ord a, Num a, Fractional t) =>
    t ->
    t ->
    (t -> a) ->
    t ->
    [(t, t)]
separateRoots a b f n = helper a
  where
    h = (b - a) / n
    helper leftBound
        | leftBound >= b = []
        | (f leftBound * f (leftBound + h)) <= 0 =
            (leftBound, leftBound + h) : helper (leftBound + h)
        | otherwise = helper (leftBound + h)

approximateByBisection ::
    Double ->
    Double ->
    (Double -> Double) ->
    Double ->
    Int ->
    MyResult
approximateByBisection a b f epsilon counter'
    | f a == 0 = MyResult a 0 0 (counter' + 1)
    | f c == 0 = MyResult c 0 0 (counter' + 1)
    | (f a * f c) < 0 =
        if c - a > 2 * epsilon
            then approximateByBisection a c f epsilon (counter' + 1)
            else MyResult ((a + c) / 2) ((c - a) / 2) (abs (f ((a + c) / 2))) (counter' + 1)
    | otherwise =
        if b - c > 2 * epsilon
            then approximateByBisection c b f epsilon (counter' + 1)
            else MyResult ((c + b) / 2) ((b - c) / 2) (abs (f ((c + b) / 2))) (counter' + 1)
  where
    c = (a + b) / 2

approximateByNewton ::
    Double ->
    Double ->
    (Double -> Double) ->
    (Double -> Double) ->
    Double ->
    MyResult
approximateByNewton a b f f' epsilon = helper x 0
  where
    x = (a + b) / 2
    helper x' counter'
        -- \| f' x == 0 || f x * f'' x <= 0 = helper (findNewtonStartingPoints f a b (10 ** (-6))) f epsilon counter
        | f x_next == 0 = MyResult x_next 0 0 (counter' + 1)
        | abs (x_next - x') <= epsilon =
            MyResult x_next (abs (x_next - x')) (abs (f x_next)) (counter' + 1)
        | otherwise = helper x_next (counter' + 1)
      where
        x_next = x' - f x' / f' x'

approximateByModifiedNewton ::
    Double ->
    Double ->
    (Double -> Double) ->
    (Double -> Double) ->
    Double ->
    MyResult
approximateByModifiedNewton a b f f' epsilon = helper x 0 x
  where
    x = (a + b) / 2
    helper x' counter' x_0
        | f x_next == 0 = MyResult x_next 0 0 (counter' + 1)
        | abs (x_next - x') <= epsilon =
            MyResult x_next (abs (x_next - x')) (abs (f x_next)) (counter' + 1)
        | otherwise = helper x_next (counter' + 1) x'
      where
        x_next = x' - f x' / f' x_0

approximateBySecant ::
    Double ->
    Double ->
    (Double -> Double) ->
    Double ->
    MyResult
approximateBySecant a b f epsilon = helper a b 0
  where
    helper x x_pred counter'
        | f x_next == 0 = MyResult x_next 0 0 (counter' + 1)
        | abs (x_next - x) < epsilon =
            MyResult x_next (abs (x_next - x)) (abs (f x_next)) (counter' + 1)
        | otherwise = helper x_next x (counter' + 1)
      where
        x_next = x - (f x / (f x - f x_pred)) * (x - x_pred)
