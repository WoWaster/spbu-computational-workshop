module Integration (
    leftRule,
    rightRule,
    midpointRule,
    trapezoidalRule,
    simpsonsRule,
    threeEightsRule,
    exactIntegral,
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
