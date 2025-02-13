// Реализация с использованием хвостовой рекурсии
let rec maxPathSumTail (triangle: int list list) rowIndex (acc: int list) : int =
    match rowIndex with
    | -1 -> acc.[0] // Возвращаем верхнюю вершину, где накоплена максимальная сумма
    | _ ->
        let updatedAcc =
            [ for i in 0 .. (List.length triangle.[rowIndex] - 1) ->
                max (acc.[i]) (acc.[i + 1]) + triangle.[rowIndex].[i] ]
        maxPathSumTail triangle (rowIndex - 1) updatedAcc

let maxPathSumTailWrapper (triangle: int list list) : int =
    let lastRow = List.last triangle
    maxPathSumTail triangle (List.length triangle - 2) lastRow

// Реализация с использованием обычной рекурсии
let rec maxPathSumRecursive (triangle: int list list) rowIndex colIndex : int =
    if rowIndex = List.length triangle - 1 then
        triangle.[rowIndex].[colIndex]
    else
        triangle.[rowIndex].[colIndex] +
        max (maxPathSumRecursive triangle (rowIndex + 1) colIndex)
            (maxPathSumRecursive triangle (rowIndex + 1) (colIndex + 1))

let maxPathSumRecursiveWrapper (triangle: int list list) : int =
    maxPathSumRecursive triangle 0 0

// Модульная реализация с использованием reduce
let maxPathSumModular (triangle: int list list) : int =
    triangle
    |> List.rev
    |> List.reduce (fun (acc: int list) (row: int list) ->
        [ for i in 0 .. (List.length row - 1) ->
            row.[i] + max acc.[i] acc.[i + 1] ])
    |> List.head


let triangle: int list list = [
    [75]
    [95; 64]
    [17; 47; 82]
    [18; 35; 87; 10]
    [20; 4; 82; 47; 65]
    [19; 1; 23; 75; 3; 34]
    [88; 2; 77; 73; 7; 63; 67]
    [99; 65; 4; 28; 6; 16; 70; 92]
    [41; 41; 26; 56; 83; 40; 80; 70; 33]
    [41; 48; 72; 33; 47; 32; 37; 16; 94; 29]
    [53; 71; 44; 65; 25; 43; 91; 52; 97; 51; 14]
    [70; 11; 33; 28; 77; 73; 17; 78; 39; 68; 17; 57]
    [91; 71; 52; 38; 17; 14; 91; 43; 58; 50; 27; 29; 48]
    [63; 66; 4; 68; 89; 53; 67; 30; 73; 16; 69; 87; 40; 31]
    [4; 62; 98; 27; 23; 9; 70; 98; 73; 93; 38; 53; 60; 4; 23]
]

printfn "Хвостовая рекурсия: %d" (maxPathSumTailWrapper triangle)
printfn "Рекурсия: %d" (maxPathSumRecursiveWrapper triangle)
printfn "Модульная реализация: %d" (maxPathSumModular triangle)

// Тест для сравнения всех решений
let compareSolutions (triangle: int list list) : unit =
    let results = [
        maxPathSumTailWrapper triangle
        maxPathSumRecursiveWrapper triangle
        maxPathSumModular triangle
    ]

    if results |> List.distinct |> List.length = 1 then
        printfn "Все реализации возвращают одинаковый результат: %d" results.Head
    else
        printfn "Результаты различаются: %A" results

compareSolutions triangle
