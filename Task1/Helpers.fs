module Helpers

let printNumberedPairsn number (leftBound, rightBound) =
    printfn "%d. [%e; %e]" (number + 1) leftBound rightBound

let printBisectionApproximationInfo
    number
    startingApproximation
    (iterations, finalApproximation, lastIntervalLength, residual)
    =
    let message =
        Printf.TextWriterFormat<int -> float -> int -> float -> float -> float -> unit>(
            """Корень из отрезка №%d:
Начальное приближение: %e
Количество шагов: %d
Приближенное решение с точностью ε: %e
Длина отрезка при последнем приближении: %e
Абсолютная величина невязки: %e"""
        )

    printfn message (number + 1) startingApproximation iterations finalApproximation lastIntervalLength residual

let printSequenceApproximationInfo
    number
    startingApproximation
    (iterations, finalApproximation, lastIntervalLength, residual)
    =
    let message =
        Printf.TextWriterFormat<int -> float -> int -> float -> float -> float -> unit>(
            """Корень из отрезка №%d:
Начальное приближение: %e
Количество шагов: %d
Приближенное решение с точностью ε: %e
Модуль разности n-го и (n-1)-го приближений: %e
Абсолютная величина невязки: %e"""
        )

    printfn message (number + 1) startingApproximation iterations finalApproximation lastIntervalLength residual
