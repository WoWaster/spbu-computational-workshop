module Integration (
    leftRule,
    rightRule,
    midpointRule,
    trapezoidalRule,
    simpsonsRule,
    threeEightsRule,
    exactIntegral,
    genPoints,
    findW,
    findQ,
    findZ,
    leftRuleComposite,
    rightRuleComposite,
    midpointRuleComposite,
    trapezoidalRuleCompound,
    simpsonsRuleCompound,
    rungeMethod,
) where

leftRule :: (Floating t) => (t -> t) -> t -> t -> t
leftRule f leftBound rightBound = (rightBound - leftBound) * f leftBound

rightRule :: (Floating t) => (t -> t) -> t -> t -> t
rightRule f leftBound rightBound = (rightBound - leftBound) * f rightBound

midpointRule :: (Floating t) => (t -> t) -> t -> t -> t
midpointRule f leftBound rightBound = (rightBound - leftBound) * f ((leftBound + rightBound) / 2)

trapezoidalRule :: (Floating t) => (t -> t) -> t -> t -> t
trapezoidalRule f leftBound rightBound = (rightBound - leftBound) / 2 * (f leftBound + f rightBound)

simpsonsRule :: (Floating t) => (t -> t) -> t -> t -> t
simpsonsRule f leftBound rightBound =
    (rightBound - leftBound)
        / 6
        * (f leftBound + 4 * f ((leftBound + rightBound) / 2) + f rightBound)

threeEightsRule :: (Floating t) => (t -> t) -> t -> t -> t
threeEightsRule f leftBound rightBound =
    (rightBound - leftBound)
        * ( f leftBound / 8
                + 3 / 8 * f (leftBound + h)
                + 3 / 8 * f (leftBound + 2 * h)
                + f rightBound / 8
          )
  where
    h = (rightBound - leftBound) / 3

exactIntegral :: (Floating t) => (t -> t) -> t -> t -> t
exactIntegral fBig leftBound rightBound = fBig rightBound - fBig leftBound

genPoints :: (Floating t) => t -> t -> Int -> ([t], t)
genPoints leftBound rightBound nOfPoints =
    ( [leftBound + fromIntegral i * difference | i <- [0 .. nOfPoints]]
    , difference
    )
  where
    difference = (rightBound - leftBound) / fromIntegral nOfPoints

findW :: (Floating t) => (t -> t) -> [t] -> t
findW _ [] = error "Количество точек не может быть меньше двух!"
findW _ [_] = error "Количество точек не может быть меньше двух!"
findW f (_ : points) = sum $ map f $ init points

findQ :: (Fractional t) => (t -> t) -> t -> [t] -> t
findQ _ _ [] = error "Количество точек не может быть меньше одной!"
findQ f difference points = sum $ map (\x -> f (x + difference / 2)) $ init points

findZ :: (Floating t) => (t -> t) -> [t] -> t
findZ _ [] = error "Количество точек не может быть меньше двух!"
findZ _ [_] = error "Количество точек не может быть меньше двух!"
findZ f points = f (head points) + f (last points)

leftRuleComposite :: (Floating t) => t -> t -> t -> t
leftRuleComposite fZero difference w = difference * (fZero + w)

rightRuleComposite :: (Floating t) => t -> t -> t -> t
rightRuleComposite fLast difference w = difference * (w + fLast)

midpointRuleComposite :: (Floating t) => t -> t -> t
midpointRuleComposite difference q = difference * q

trapezoidalRuleCompound :: (Floating t) => t -> t -> t -> t
trapezoidalRuleCompound difference w z = difference / 2 * (z + 2 * w)

simpsonsRuleCompound :: (Floating t) => t -> t -> t -> t -> t
simpsonsRuleCompound difference w q z = difference / 6 * (z + 2 * w + 4 * q)

rungeMethod :: (Floating t, Integral p) => t -> t -> p -> p -> t
rungeMethod valueH valueHl degree multiplier =
    (fromIntegral multiplier ** r * valueHl - valueH)
        / (fromIntegral multiplier ** r - 1)
  where
    r = fromIntegral degree + 1
