module Interpolation

type InterpolationMethod =
    | Lagrange of power: int16
    | Linear

let getPointSize m =
    match m with
    | Lagrange pow -> int pow
    | Linear -> 2

let interpolation method points =
    match method with
    | Linear ->
        fun x ->
            let x1 = points |> Seq.head |> fst
            let x2 = points |> Seq.last |> fst
            let y1 = points |> Seq.head |> snd
            let y2 = points |> Seq.last |> snd
            y1 + (x - x1) * (y2 - y1) / (x2 - x1)
    | Lagrange power ->
        fun x ->
            points
            |> Seq.mapi (fun idx (x_i, y_i) ->
                y_i
                * (points
                   |> Seq.removeAt idx
                   |> Seq.map (fun (x_j, y_j) -> (x - x_j) / (x_i - x_j))
                   |> Seq.reduce (*)))
            |> Seq.sum

let interpolate method step points =
    let startX, startY = Seq.head points
    let finishX, finishY = Seq.last points
    Seq.initInfinite (fun x -> startX + step * float x)
    |> Seq.takeWhile (fun x -> x <= finishX)
    |> Seq.map (fun x -> x, interpolation method points x)

let pointedMap processors nums =
    let maxPointsSize = fst (processors |> Seq.maxBy fst)
    nums
    |> Seq.scan
        (fun (prevPoint, _) x ->
            let points =
                if List.length prevPoint < maxPointsSize then
                    List.append prevPoint [ x ]
                else
                    List.append (List.tail prevPoint) [ x ]
            let outSeq =
                processors
                |> Seq.map (fun (getPointSize, proc) ->
                    if getPointSize <= List.length points then
                        Some(points |> List.skip (List.length points - getPointSize) |> proc)
                    else
                        None)
            points, outSeq)
        (List.empty, Seq.empty)
    |> Seq.map snd

let interpolateStream data methods step =
    let processors =
        methods
        |> Seq.map (fun m -> getPointSize m, fun points -> interpolate m step points)
    data |> pointedMap processors |> Seq.skip 1