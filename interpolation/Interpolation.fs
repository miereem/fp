module Interpolation

// Тип, который определяет два метода интерполяции: Лагранжа и Линейный
type InterpolationMethod =
    | Lagrange of power: int16  // Интерполяция Лагранжа с заданной степенью (количеством точек)
    | Linear  // Линейная интерполяция (для 2 точек)

// Функция для получения числа точек, которое требуется для метода интерполяции
let getPointSize m =
    match m with
    | Lagrange pow -> int pow  // Для Лагранжа это число точек (power)
    | Linear -> 2  // Для линейной интерполяции достаточно 2 точек

// Выполняет интерполяцию по заданному методу на основе набора точек
let interpolation method points =
    match method with
    | Linear ->  // Линейная интерполяция
        fun x ->  // Возвращает функцию, которая рассчитывает y для заданного x
            let x1 = points |> Seq.head |> fst  // Первая точка (x1, y1)
            let x2 = points |> Seq.last |> fst  // Последняя точка (x2, y2)
            let y1 = points |> Seq.head |> snd  // Первая точка (y1)
            let y2 = points |> Seq.last |> snd  // Последняя точка (y2)

            // Формула линейной интерполяции
            y1 + (x - x1) * (y2 - y1) / (x2 - x1)
    | Lagrange power ->  // Интерполяция Лагранжаdfd
        fun x ->  // Возвращает функцию, которая рассчитывает y для заданного x
            points
            |> Seq.mapi (fun idx (x_i, y_i) ->  // Для каждой точки в наборе
                y_i
                * (points
                   |> Seq.removeAt idx  // Убираем текущую точку, чтобы использовать ее в формуле Лагранжа
                   |> Seq.map (fun (x_j, y_j) -> (x - x_j) / (x_i - x_j))  // Строим множители для формулы Лагранжа
                   |> Seq.reduce (*)))  // Перемножаем все множители для получения итогового значения
            |> Seq.sum 
             // Суммируем все значения и получаем итоговый результат интерполяции

// Функция для выполнения интерполяции с заданным шагом для набора точек
let interpolate method step points =
    let startX, startY = Seq.head points  // Первая точка (x1, y1)
    let finishX, finishY = Seq.last points  // Последняя точка (x2, y2)

    // Генерируем последовательность точек с шагом step от startX до finishX
    Seq.initInfinite (fun x -> startX + step * float x)
    |> Seq.takeWhile (fun x -> x <= finishX)  // Берем только те точки, которые не превышают finishX
    |> Seq.map (fun x -> x, interpolation method points x)  // Для каждой точки вычисляем интерполяцию и возвращаем пару (x, y)

// Функция для обработки потока данных с применением нескольких процессоров (интерполяций)
let pointedMap processors nums =
    // Определяем максимальный размер окна, который требуется для одного из процессоров
    let maxPointsSize = fst (processors |> Seq.maxBy fst)

    nums
    |> Seq.scan
        (fun (prevPoint, _) x ->  // Для каждого числа в nums строим новое окно точек
            let points =
                if List.length prevPoint < maxPointsSize then
                    // Если окно еще не заполнилось, добавляем текущую точку
                    List.append prevPoint [ x ]
                else
                    // Если окно заполнено, сдвигаем его, убирая первую точку
                    List.append (List.tail prevPoint) [ x ]

            let outSeq =
                processors
                |> Seq.map (fun (getPointSize, proc) ->  // Для каждого процессора
                    if getPointSize <= List.length points then
                        // Если текущее окно достаточно большое для этого процессора
                        Some(points |> List.skip (List.length points - getPointSize) |> proc)  // Применяем процессор
                    else
                        None)  // Если окно слишком маленькое для этого процессора, возвращаем None

            points, outSeq)  // Возвращаем обновленное окно и результат работы процессоров
        (List.empty, Seq.empty)  // Начальное состояние: пустое окно и пустая последовательность результатов
    |> Seq.map snd  // Извлекаем только результаты работы процессоров

// Функция для интерполяции потока данных с заданными методами интерполяции
let interpolateStream data methods step =
    // Создаем процессоры для каждого метода интерполяции
    let processors =
        methods
        |> Seq.map (fun m -> getPointSize m, fun points -> interpolate m step points)

    // Применяем pointedMap для потока данных с процессорами и пропускаем первое значение
    data |> pointedMap processors |> Seq.skip 1