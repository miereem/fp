# Лабораторная работа №3

Выполнила: Абдурасул кызы Мээрим, P34092

## Задание

Цель: получить навыки работы с вводом/выводом, потоковой обработкой данных, командной строкой.
В рамках лабораторной работы вам предлагается повторно реализовать лабораторную работу по предмету "Вычислительная математика" посвящённую интерполяции (в разные годы это лабораторная работа 3 или 4) со следующими дополнениями:

- обязательно должна быть реализована линейная интерполяция (отрезками, link);
- настройки алгоритма интерполяции и выводимых данных должны задаваться через аргументы командной строки:
  - какие алгоритмы использовать (в том числе два сразу);
  - частота дискретизации результирующих данных;
  - и т.п.;
- входные данные должны задаваться в текстовом формате на подобии ".csv" (к примеру x;y\n или x\ty\n) и подаваться на стандартный ввод, входные данные должны быть отсортированы по возрастанию x;
- выходные данные должны подаваться на стандартный вывод;
- программа должна работать в потоковом режиме (пример -- cat | grep 11), это значит, что при запуске программы она должна ожидать получения данных на стандартный ввод, и, по мере получения достаточного количества данных, должна выводить рассчитанные точки в стандартный вывод;


## Аспекты реализации

### Модуль интерполяции
```
module Interpolation

type InterpolationMethod =
    | Lagrange of power: int16
    | Linear

// Количество точек для методов
let getPointSize m =
    match m with
    | Lagrange pow -> int pow
    | Linear -> 2

// Выполняет интерполяцию на основе заданного метода и набора точек.
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

// Выполняет интерполяцию с шагом step для заданных точек
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
```

### Модуль тестирования

```
module Lab4.Tests

open System

open Xunit
open FsCheck.Xunit
open Interpolation
open System.Collections.Generic

let checkFunction f methods points =
    interpolateStream points methods 0.1
    |> Seq.iter (fun seq ->
        seq
        |> Seq.iter (fun possiblePoints ->
            match possiblePoints with
            | Some seq -> seq |> Seq.iter (fun (x, y) -> Assert.True(abs (f x - y) <= 0.1))
            | None -> ()))

[<Property>]
let ``Check linear function`` (k: int, l: int list) =
    let f = fun (x: float) -> float k * x

    l
    |> List.sort
    |> List.distinct
    |> List.map (fun x -> (float x, float (f x)))
    |> checkFunction f [ Linear; Lagrange(int16 2) ]

[<Property>]
let ``Check square function`` (l: int list) =
    let f = fun (x: float) -> float x * x

    l
    |> List.sort
    |> List.distinct
    |> List.map (fun x -> (float x, float (f x)))
    |> checkFunction f [ Lagrange(int16 3) ]


[<Property>]
let ``Check cubic function`` (l: int list) =
    let f = fun (x: float) -> x ** 3.0

    l
    |> List.sort
    |> List.distinct
    |> List.map (fun x -> (float x, float (f x)))
    |> checkFunction f [ Lagrange(int16 4) ]

[<Property>]
let ``Check function pow 4`` (l: int list) =
    let f = fun (x: float) -> x ** 4.0

    l
    |> List.sort
    |> List.distinct
    |> List.map (fun x -> (float x, float (f x)))
    |> checkFunction f [ Lagrange(int16 5) ]


```

Информация о тестировании:
```
Пройден!   : не пройдено     0, пройдено     4, пропущено     0, всего     4, длительность 229 ms. - tests.dll (net8.0)
```

## Ввод/вывод программы

Пример вычислений для шага 1.0 и функции sin(x):
```
dotnet run 1.0 linear lagrange=4

1. Chosen step: 1.00
2. Chosen methods: seq [Linear; Lagrange 4s]
0 0.00
1.571 1
Linear
0.00    1.00
0.00    0.64

3.142 0
Linear
1.57    2.57
1.00    0.36

4.712 -1
Linear
3.14    4.14
0.00    -0.64

Lagrange 4s
0.00    1.00    2.00    3.00    4.00
0.00    0.97    0.84    0.12    -0.67

12.568 0
Linear
4.71    5.71    6.71    7.71    8.71    9.71    10.71   11.71
-1.00   -0.87   -0.75   -0.62   -0.49   -0.36   -0.24   -0.11

Lagrange 4s
1.57    2.57    3.57    4.57    5.57    6.57    7.57    8.57    9.57    10.57       11.57
1.00    0.37    -0.28   -0.91   -1.49   -1.95   -2.26   -2.38   -2.25   -1.84       -1.11
```

## Вывод

В ходе выполнения лабораторной работы я освоила работу с потоковыми данными и реализовала два метода интерполяции: линейную интерполяцию и интерполяцию Лагранжа. Все задачи необходимо было выполнять через командную строку, при этом данные поступали в программу постепенно. 

Основные трудности, с которыми я столкнулась:

Организация обработки потоковых данных.

Реализация сдвига окон для интерполяции Лагранжа.

Эта работа позволила глубже понять принципы работы с потоковыми данными и методами интерполяции.