module Helpers

let printNumberedPairsn number (leftBound, rightBound) =
    printfn "%d. [%f; %f]" (number + 1) leftBound rightBound

let printBisectionApproximationInfo
    startingApproximation
    (iterations, finalApproximation, lastIntervalLength, residual)
    =
    let message =
        Printf.TextWriterFormat<float -> int -> float -> float -> float -> unit>(
            """Начальное приближение: %.15e
Количество шагов: %d
Приближенное решение с точностью ε: %.15e
Длина отрезка при последнем приближении: %.15e
Абсолютная величина невязки: %.15e"""
        )

    printfn message startingApproximation iterations finalApproximation lastIntervalLength residual

let printSequenceApproximationInfo
    startingApproximation
    (iterations, finalApproximation, lastIntervalLength, residual)
    =
    let message =
        Printf.TextWriterFormat<float -> int -> float -> float -> float -> unit>(
            """Начальное приближение: %.15e
Количество шагов: %d
Приближенное решение с точностью ε: %.15e
Модуль разности n-го и (n-1)-го приближений: %.15e
Абсолютная величина невязки: %.15e"""
        )

    printfn message startingApproximation iterations finalApproximation lastIntervalLength residual
