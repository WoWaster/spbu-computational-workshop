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

let bisectionStartingApproximations =
    List.map (fun (leftBound, rightBound) -> (leftBound + rightBound) / 2.0) rootIntervals

let bisectionApproximations =
    List.map (fun (leftBound, rightBound) -> approximateByBisection f leftBound rightBound epsilon) rootIntervals

let newtonStartingApproximations =
    List.map (fun (leftBound, rightBound) -> findNewtonStartingPoints f f'' leftBound rightBound epsilon) rootIntervals

let newtonApproximations =
    List.map (fun startingPoint -> approximateByNewton f f' epsilon startingPoint) newtonStartingApproximations

let modifiedNewtonApproximations =
    List.map
        (fun startingPoint -> approximateByModifiedNewton f epsilon startingPoint (f' startingPoint))
        newtonStartingApproximations

let secantStartingApproximations =
    List.map (fun (leftBound, rightBound) -> secantApproximation f leftBound rightBound) rootIntervals

let secantApproximations =
    List.map (fun (leftBound, rightBound) -> approximateBySecant f leftBound rightBound epsilon) rootIntervals

// Main execution
printfn "Тема: ЧИСЛЕННЫЕ МЕТОДЫ РЕШЕНИЯ НЕЛИНЕЙНЫХ УРАВНЕНИЙ"
printfn "Входные данные:"
printfn "A = %g, B = %g, ε = %g, f(x) = (x-1)^2 - exp(-x)" leftBound rightBound epsilon

printfn "Для отделения корней использовались N = %g и h = %g" intervalCount difference
printfn "Найдено отрезков: %d" (List.length rootIntervals)
List.iteri printNumberedPairsn rootIntervals
printfn ""

for i = 0 to ((List.length rootIntervals) - 1) do
    printfn "════════════════════════════════════════════════════════════════════════════════"

    printfn
        "Уточним корни на отрезке №%d: [%f; %f]"
        (i + 1)
        (fst (List.item i rootIntervals))
        (snd (List.item i rootIntervals))

    printfn "────────────────────────────────────────────────────────────────────────────────"
    printfn "При уточнении корней методом бисекции получены следующие результаты:"
    printBisectionApproximationInfo (List.item i bisectionStartingApproximations) (List.item i bisectionApproximations)
    printfn "────────────────────────────────────────────────────────────────────────────────"
    printfn "При уточнении корней методом Ньютона получены следующие результаты:"
    printSequenceApproximationInfo (List.item i newtonStartingApproximations) (List.item i newtonApproximations)
    printfn "────────────────────────────────────────────────────────────────────────────────"
    printfn "При уточнении корней модифицированным методом Ньютона получены следующие результаты:"
    printSequenceApproximationInfo (List.item i newtonStartingApproximations) (List.item i modifiedNewtonApproximations)
    printfn "────────────────────────────────────────────────────────────────────────────────"
    printfn "При уточнении корней методом секущих получены следующие результаты:"
    printSequenceApproximationInfo (List.item i secantStartingApproximations) (List.item i secantApproximations)
    printfn "════════════════════════════════════════════════════════════════════════════════\n"
