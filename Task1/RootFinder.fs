module RootFinder

let hasRootInInterval f leftBound rightBound =
    let leftValue = f leftBound
    let rightValue = f rightBound

    match sign (leftValue * rightValue) with
    | 1 -> false
    | 0
    | -1 -> true
    | _ -> failwith "Impossible outcome of sign!"


let separateRoots (f: float -> float) leftBound rightBound difference =
    let rec helper leftBound intervals =
        if leftBound >= rightBound then
            intervals
        else
            let rightBound = leftBound + difference

            if hasRootInInterval f leftBound rightBound then
                helper rightBound ((leftBound, rightBound) :: intervals)
            else
                helper rightBound intervals in

    List.rev (helper leftBound [])

let approximateByBisection f leftBound rightBound precision =
    let rec helper leftBound rightBound iterations =
        let middlePoint = (leftBound + rightBound) / 2.0

        if rightBound - leftBound <= 2.0 * precision then
            (iterations, middlePoint, rightBound - leftBound, abs (f middlePoint))
        else if hasRootInInterval f leftBound middlePoint then
            helper leftBound middlePoint (iterations + 1)
        else
            helper middlePoint rightBound (iterations + 1) in

    helper leftBound rightBound 0

let findNewtonStartingPoints (f: float -> float) f'' leftBound rightBound precision =
    let rec helper leftBound =
        if leftBound > rightBound then
            failwith "Не могу найти начальную точку для метода Ньютона."
        else if ((f leftBound) * (f'' leftBound) > 0.0) then
            leftBound
        else
            helper (leftBound + precision) in

    helper leftBound


let approximateByNewton (f: float -> float) f' precision startingPoint =
    let rec helper startingPoint iterations =
        let newStartingPoint = startingPoint - ((f startingPoint) / (f' startingPoint))
        let difference = abs (newStartingPoint - startingPoint)

        if difference < precision then
            (iterations, newStartingPoint, difference, abs (f newStartingPoint))
        else
            helper newStartingPoint (iterations + 1) in

    helper startingPoint 1

let approximateByModifiedNewton (f: float -> float) precision startingPoint f'OfStartingPoint =
    let rec helper startingPoint iterations =
        printfn "Entered newton helper with point %e and iter %d" startingPoint iterations

        let newStartingPoint = startingPoint - ((f startingPoint) / f'OfStartingPoint)
        let difference = abs (newStartingPoint - startingPoint)

        if difference < precision then
            (iterations, newStartingPoint, difference, abs (f newStartingPoint))
        else
            helper newStartingPoint (iterations + 1) in

    helper startingPoint 1

let secantApproximation (f: float -> float) xkPrev xk =
    xk - ((f xk) / (f xk - f xkPrev)) * (xk - xkPrev)

let approximateBySecant (f: float -> float) x0 x1 precision =
    let rec helper xkPrev xk iterations =
        let newXk = secantApproximation f xk xkPrev
        let difference = abs (newXk - xk)

        if difference < precision then
            (iterations, newXk, difference, abs (f newXk))
        else
            helper xk newXk (iterations + 1) in

    helper x0 x1 1
