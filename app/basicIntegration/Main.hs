module Main (main) where

import Input
import Integration
import Text.Layout.Table

fSimple :: (String, Double -> Double, Double -> Double)
fSimple =
    ( "x * exp x * sin x"
    , \x -> x * exp x * sin x
    , \x -> exp x / 2 * (x * (sin x - cos x) + cos x)
    )

pZero :: (String, Double -> Double, Double -> Double)
pZero = ("34", const 34, \x -> 34 * x)

pOne :: (String, Double -> Double, Double -> Double)
pOne = ("x", \x -> x, \x -> x ** 2 / 2)

pTwo :: (String, Double -> Double, Double -> Double)
pTwo =
    ( "12.6 * x ** 2 + 6.4 * x + 3.2"
    , \x -> 12.6 * x ** 2 + 6.4 * x + 3.2
    , \x -> 4.2 * x ** 3 + 3.2 * x ** 2 + 3.2 * x
    )

pThree :: (String, Double -> Double, Double -> Double)
pThree =
    ( "x ** 3 - 7 * x + 7"
    , \x -> x ** 3 - 7 * x + 7
    , \x -> x ** 4 / 4 - 7 / 2 * x ** 2 + 7 * x
    )

prettyResultTable ::
    (Double -> Double) -> (Double -> Double) -> Double -> Double -> String
prettyResultTable f fBig leftBound rightBound =
    tableString $
        columnHeaderTableS
            [def, numCol, numCol]
            unicodeRoundS
            ( titlesH
                [ "Метод"
                , "Вычисленное значение"
                , "Абсолютная погрешность"
                ]
            )
            [ rowG ["Точное значение", show actualValue, show $ absError actualValue]
            , rowG
                [ "КФ левого прямоугольника"
                , show leftRuleValue
                , show $ absError leftRuleValue
                ]
            , rowG
                [ "КФ правого прямоугольника"
                , show rightRuleValue
                , show $ absError rightRuleValue
                ]
            , rowG
                [ "КФ среднего прямоугольника"
                , show midpointRuleValue
                , show $ absError midpointRuleValue
                ]
            , rowG
                [ "КФ трапеции"
                , show trapezoidalRuleValue
                , show $ absError trapezoidalRuleValue
                ]
            , rowG
                [ "КФ Симпсона"
                , show simpsonsRuleValue
                , show $ absError simpsonsRuleValue
                ]
            , rowG
                [ "КФ 3/8"
                , show threeEightsRuleValue
                , show $ absError threeEightsRuleValue
                ]
            ]
  where
    actualValue = exactIntegral fBig leftBound rightBound
    absError x = abs $ x - actualValue
    leftRuleValue = leftRule f leftBound rightBound
    rightRuleValue = rightRule f leftBound rightBound
    midpointRuleValue = midpointRule f leftBound rightBound
    trapezoidalRuleValue = trapezoidalRule f leftBound rightBound
    simpsonsRuleValue = simpsonsRule f leftBound rightBound
    threeEightsRuleValue = threeEightsRule f leftBound rightBound

printFuncResult ::
    (String, Double -> Double, Double -> Double) -> Double -> Double -> IO ()
printFuncResult (name, f, fBig) leftBound rightBound = do
    putStrLn ""
    putStrLn $ "f(x) = " ++ name
    putStrLn $ prettyResultTable f fBig leftBound rightBound

main :: IO ()
main = do
    putStrLn "Тема: Приближённое вычисление интеграла по квадратурным формулам"
    leftBound <- inputDouble "Введите левую границу интеграла: "
    rightBound <- inputDouble "Введите правую границу интеграла: "
    mapM_
        (\fInfo -> printFuncResult fInfo leftBound rightBound)
        [fSimple, pZero, pOne, pTwo, pThree]
