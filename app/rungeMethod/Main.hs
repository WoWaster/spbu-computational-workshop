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
    (Double -> Double) ->
    (Double -> Double) ->
    Double ->
    Double ->
    Int ->
    Int ->
    String
prettyResultTable f fBig leftBound rightBound nOfPoints multiplier =
    tableString $
        columnHeaderTableS
            [def, numCol, numCol, numCol, numCol, numCol, numCol]
            unicodeRoundS
            ( titlesH
                [ "Метод"
                , "Вычисленное значение при h"
                , "Абсолютная погрешность"
                , "Вычисленное значение при h/l"
                , "Абсолютная погрешность"
                , "Уточненное значение"
                , "Абсолютная погрешность"
                ]
            )
            [ rowG
                [ "Точное значение"
                , show actualValue
                , show $ absError actualValue
                , show actualValue
                , show $ absError actualValue
                , show actualValue
                , show $ absError actualValue
                ]
            , rowG
                [ "КФ левого прямоугольника"
                , show leftRuleValueH
                , show $ absError leftRuleValueH
                , show leftRuleValueHl
                , show $ absError leftRuleValueHl
                , show leftRuleRunge
                , show $ absError leftRuleRunge
                ]
            , rowG
                [ "КФ правого прямоугольника"
                , show rightRuleValueH
                , show $ absError rightRuleValueH
                , show rightRuleValueHl
                , show $ absError rightRuleValueHl
                , show rightRuleRunge
                , show $ absError rightRuleRunge
                ]
            , rowG
                [ "КФ среднего прямоугольника"
                , show midpointRuleValueH
                , show $ absError midpointRuleValueH
                , show midpointRuleValueHl
                , show $ absError midpointRuleValueHl
                , show midpointRuleRunge
                , show $ absError midpointRuleRunge
                ]
            , rowG
                [ "КФ трапеции"
                , show trapezoidalRuleValueH
                , show $ absError trapezoidalRuleValueH
                , show trapezoidalRuleValueHl
                , show $ absError trapezoidalRuleValueHl
                , show trapezoidalRuleRunge
                , show $ absError trapezoidalRuleRunge
                ]
            , rowG
                [ "КФ Симпсона"
                , show simpsonsRuleValueH
                , show $ absError simpsonsRuleValueH
                , show simpsonsRuleValueHl
                , show $ absError simpsonsRuleValueHl
                , show simpsonsRuleRunge
                , show $ absError simpsonsRuleRunge
                ]
            ]
  where
    actualValue = exactIntegral fBig leftBound rightBound
    absError x = abs $ x - actualValue
    (pointsH, differenceH) = genPoints leftBound rightBound nOfPoints
    wH = findW f pointsH
    qH = findQ f differenceH pointsH
    zH = findZ f pointsH
    leftRuleValueH = leftRuleComposite (f (head pointsH)) differenceH wH
    rightRuleValueH = rightRuleComposite (f (last pointsH)) differenceH wH
    midpointRuleValueH = midpointRuleComposite differenceH qH
    trapezoidalRuleValueH = trapezoidalRuleCompound differenceH wH zH
    simpsonsRuleValueH = simpsonsRuleCompound differenceH wH qH zH
    (pointsHl, differenceHl) = genPoints leftBound rightBound (nOfPoints * multiplier)
    wHl = findW f pointsHl
    qHl = findQ f differenceHl pointsHl
    zHl = findZ f pointsHl
    leftRuleValueHl = leftRuleComposite (f (head pointsHl)) differenceHl wHl
    rightRuleValueHl = rightRuleComposite (f (last pointsHl)) differenceHl wHl
    midpointRuleValueHl = midpointRuleComposite differenceHl qHl
    trapezoidalRuleValueHl = trapezoidalRuleCompound differenceHl wHl zHl
    simpsonsRuleValueHl = simpsonsRuleCompound differenceHl wHl qHl zHl
    leftRuleRunge = rungeMethod leftRuleValueH leftRuleValueHl 0 multiplier
    rightRuleRunge = rungeMethod rightRuleValueH rightRuleValueHl 0 multiplier
    midpointRuleRunge = rungeMethod midpointRuleValueH midpointRuleValueHl 1 multiplier
    trapezoidalRuleRunge = rungeMethod trapezoidalRuleValueH trapezoidalRuleValueHl 1 multiplier
    simpsonsRuleRunge = rungeMethod simpsonsRuleValueH simpsonsRuleValueHl 3 multiplier

printFuncResult ::
    (String, Double -> Double, Double -> Double) ->
    Double ->
    Double ->
    Int ->
    Int ->
    IO ()
printFuncResult (name, f, fBig) leftBound rightBound nOfPoints multiplier = do
    putStrLn ""
    putStrLn $ "f(x) = " ++ name
    putStrLn $ prettyResultTable f fBig leftBound rightBound nOfPoints multiplier

main :: IO ()
main = do
    putStrLn
        "Тема: Приближённое вычисление интеграла по составным квадратурным формулам"
    leftBound <- inputDouble "Введите левую границу интеграла: "
    rightBound <- inputDouble "Введите правую границу интеграла: "
    nOfPoints <- inputInt "Введите число промежутков деления отрезка: "
    multiplier <- inputInt "Введите множитель l метода Рунге: "
    mapM_
        (\fInfo -> printFuncResult fInfo leftBound rightBound nOfPoints multiplier)
        [ fSimple
        , pZero
        , pOne
        , -- , pTwo
          pThree
        ]
