open RootFinder
open Helpers

// Input parameters
let leftBound = -1.0
let rightBound = 3.0
let epsilon = 1e-8
let intervalCount = 1e3
let f x = (x - 1.0) ** 2.0 - exp (-x)
let f' x = 2.0 * (x - 1.0) + exp (-x)
let f'' x = 2.0 - exp (-x)

// Computations
let difference = ((rightBound - leftBound) / intervalCount)
let rootIntervals = separateRoots f leftBound rightBound difference

let startingApproximations =
    List.map (fun (leftBound, rightBound) -> (leftBound + rightBound) / 2.0) rootIntervals

let bisectionApproximations =
    List.map (fun (leftBound, rightBound) -> approximateByBisection f leftBound rightBound epsilon) rootIntervals

let newtonStartingApproximations =
    List.map (fun (leftBound, rightBound) -> findNewtonStartingPoints f f'' leftBound rightBound epsilon) rootIntervals

let newtonApproximations =
    List.map (fun startingPoint -> approximateByNewton f f' epsilon startingPoint) newtonStartingApproximations

let modifiedNewtonApproximations =
    List.map (fun startingPoint -> approximateByNewton f f' epsilon startingPoint) newtonStartingApproximations

let secantStartingApproximations =
    List.map (fun (leftBound, rightBound) -> secantApproximation f leftBound rightBound) rootIntervals

let secantApproximations =
    List.map (fun (leftBound, rightBound) -> approximateBySecant f leftBound rightBound epsilon) rootIntervals

// Main execution
printfn "Тема: ЧИСЛЕННЫЕ МЕТОДЫ РЕШЕНИЯ НЕЛИНЕЙНЫХ УРАВНЕНИЙ"
printfn "Входные данные:"
printfn "A = %e, B = %e, ε = %e, f(x) = (x-1)^2 - exp(-x)" leftBound rightBound epsilon

printfn "Для отделения корней использовались N = %e и h = %e" intervalCount difference
printfn "Найдено отрезков: %d" (List.length rootIntervals)
List.iteri printNumberedPairsn rootIntervals

printfn "\nПри уточнении корней методом бисекции получены следующие результаты:"
List.iteri2 printBisectionApproximationInfo startingApproximations bisectionApproximations

printfn "\nПри уточнении корней методом Ньютона получены следующие результаты:"
List.iteri2 printSequenceApproximationInfo newtonStartingApproximations newtonApproximations

printfn "\nПри уточнении корней модифицированным методом Ньютона получены следующие результаты:"
List.iteri2 printSequenceApproximationInfo newtonStartingApproximations modifiedNewtonApproximations

printfn "\nПри уточнении корней методом секущих получены следующие результаты:"
List.iteri2 printSequenceApproximationInfo secantStartingApproximations secantApproximations
