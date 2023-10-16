module NumericalDifferentiation (
    genTable,
    makeResult,
    DifferentiationResult (..),
) where

import Control.Applicative

data DifferentiationResult = DifferentiationResult
    { xi :: Double
    , fxi :: Double
    , f'xiNum :: Double
    , difff'xiActAndNum :: Double
    , relDifff'xiActAndNum :: Double
    , f''xiNum :: Maybe Double
    , difff''xiActAndNum :: Maybe Double
    , relDifff''xiActAndNum :: Maybe Double
    }

genTable :: (Double -> Double) -> Int -> Double -> Double -> [(Double, Double)]
genTable f numOfPoints diff startingPoint =
    [ (xi, f xi) | i <- [0 .. (numOfPoints - 1)], let xi = startingPoint + fromIntegral i * diff
    ]

fstDerivativeMiddlePoint :: Double -> Double -> Double -> Double
fstDerivativeMiddlePoint fPrevX fNextX h = (fNextX - fPrevX) / (2 * h)

fstDerivativeStartPoint :: Double -> Double -> Double -> Double -> Double
fstDerivativeStartPoint fX fNextX fNextNextX h = (-3 * fX + 4 * fNextX - fNextNextX) / (2 * h)

fstDerivativeEndPoint :: Double -> Double -> Double -> Double -> Double
fstDerivativeEndPoint fPrevPrevX fPrevX fX h = (3 * fX - 4 * fPrevX + fPrevPrevX) / (2 * h)

calculateF' :: [Double] -> Double -> [Double]
calculateF' table h = helper 0 table
  where
    helper counter table'
        | counter == 0 = case table' of
            (x : y : z : t) -> fstDerivativeStartPoint x y z h : helper 1 (x : y : z : t)
            _ -> error "Необходимо хотя бы 3 точки в таблице!"
        | counter == len - 1 = case table' of
            [x, y, z] -> [fstDerivativeEndPoint x y z h]
            _ -> error "Необходимо хотя бы 3 точки в таблице!"
        | otherwise = case table' of
            t@[x, _, z] -> fstDerivativeMiddlePoint x z h : helper (counter + 1) t
            (x : y : z : t) -> fstDerivativeMiddlePoint x z h : helper (counter + 1) (y : z : t)
            _ -> error "Необходимо хотя бы 3 точки в таблице!"

    len = length table

sndDerivativeMiddlePoint :: Double -> Double -> Double -> Double -> Double
sndDerivativeMiddlePoint fPrevX fX fNextX h = (fNextX - 2 * fX + fPrevX) / (h ** 2)

calculateF'' :: [Double] -> Double -> [Maybe Double]
calculateF'' table h = helper 0 table
  where
    helper counter table'
        | counter == 0 = Nothing : helper 1 table'
        | counter == len - 1 = [Nothing]
        | otherwise = case table' of
            t@[x, y, z] -> Just (sndDerivativeMiddlePoint x y z h) : helper (counter + 1) t
            (x : y : z : t) -> Just (sndDerivativeMiddlePoint x y z h) : helper (counter + 1) (y : z : t)
            _ -> error "Необходимо хотя бы 3 точки в таблице!"

    len = length table

makeResult f' f'' table h =
    getZipList $
        DifferentiationResult
            <$> ZipList xis
            <*> ZipList fxis
            <*> ZipList f'xisN
            <*> ZipList difff's
            <*> ZipList relDifff's
            <*> ZipList f''xisN
            <*> ZipList difff''s
            <*> ZipList relDifff''s
  where
    xis = map fst table
    fxis = map snd table
    f'xisN = calculateF' fxis h
    f'xisR = map f' xis
    difff's = zipWith (\real num -> abs $ real - num) f'xisR f'xisN
    relDifff's = zipWith (\diff real -> diff / abs real) difff's f'xisR
    f''xisN = calculateF'' fxis h
    f''xisR = map f'' xis
    difff''s =
        zipWith
            ( \real num -> case num of
                Nothing -> Nothing
                Just n -> Just $ abs (real - n)
            )
            f''xisR
            f''xisN
    relDifff''s =
        zipWith
            ( \diff real -> case diff of
                Nothing -> Nothing
                Just d -> Just $ d / abs real
            )
            difff''s
            f''xisR
