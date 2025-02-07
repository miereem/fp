### Лабораторная работа №1

## Задание

Цель: освоить базовые приёмы и абстракции функционального программирования: функции, поток управления и поток данных, сопоставление с образцом, рекурсия, свёртка, отображение, работа с функциями как с данными, списки.

В рамках лабораторной работы вам предлагается решить несколько задач [проекта Эйлер](https://projecteuler.net/archives). Список задач -- ваш вариант.

Для каждой проблемы должно быть представлено несколько решений:

1. монолитные реализации с использованием:
   - хвостовой рекурсии;
   - рекурсии (вариант с хвостовой рекурсией не является примером рекурсии);
2. модульной реализации, где явно разделена генерация последовательности, фильтрация и свёртка (должны использоваться функции reduce/fold, filter и аналогичные);
3. генерация последовательности при помощи отображения (map);
4. работа со спец. синтаксисом для циклов (где применимо);
5. работа с бесконечными списками для языков, поддерживающих ленивые коллекции или итераторы как часть языка (к примеру Haskell, Clojure);
6. реализация на любом удобном для вас традиционном языке программирования для сравнения.

Требуется использовать идиоматичный для технологии стиль программирования.

## Выполнение
##### Для задачи [№18](https://projecteuler.net/problem=18) были реализованы данные варианты решения:
1. Хвостовая рекурсия
```fsharp
let rec maxPathSumTail (triangle: int list list) rowIndex (acc: int list) : int =
    match rowIndex with
    | -1 -> acc.[0] // Возвращаем верхнюю вершину, где накоплена максимальная сумма
    | _ ->
        let updatedAcc =
            [ for i in 0 .. (List.length triangle.[rowIndex] - 1) ->
                max (acc.[i]) (acc.[i + 1]) + triangle.[rowIndex].[i] ]
        maxPathSumTail triangle (rowIndex - 1) updatedAcc
```
2. Рекурсия
```fsharp
let rec maxPathSumRecursive (triangle: int list list) rowIndex colIndex : int =
    if rowIndex = List.length triangle - 1 then
        triangle.[rowIndex].[colIndex]
    else
        triangle.[rowIndex].[colIndex] +
        max (maxPathSumRecursive triangle (rowIndex + 1) colIndex)
            (maxPathSumRecursive triangle (rowIndex + 1) (colIndex + 1))
```
3. Модульная реализация reduce
```fsharp
let maxPathSumModular (triangle: int list list) : int =
    triangle
    |> List.rev
    |> List.reduce (fun (acc: int list) (row: int list) ->
        [ for i in 0 .. (List.length row - 1) ->
            row.[i] + max acc.[i] acc.[i + 1] ])
    |> List.head
```
4. Реализация на Python
```python
def compute():
	for i in reversed(range(len(triangle) - 1)):
		for j in range(len(triangle[i])):
			triangle[i][j] += max(triangle[i + 1][j], triangle[i + 1][j + 1])
	return str(triangle[0][0])
```

##### Для задачи [№13](https://projecteuler.net/problem=13) были реализованы данные варианты решения:
1. Хвостовая рекурсия
```fsharp
let sumTailRecursion (numbers: BigInteger list) =
    let rec loop acc = function
        | [] -> acc
        | hd :: tl -> loop (acc + hd) tl
    loop BigInteger.Zero numbers
```
2. Рекурсия
```fsharp
let rec sumRecursion (numbers: BigInteger list) =
    match numbers with
    | [] -> BigInteger.Zero
    | hd :: tl -> hd + sumRecursion tl
```
3. Модульная реализация fold
```fsharp
let sumWithFold numbers =
    numbers |> List.fold (+) BigInteger.Zero
```
4. Использование Seq
```fsharp
let sumWithSeq (numbers: BigInteger list) =
    numbers
    |> Seq.ofList
    |> Seq.fold (+) BigInteger.Zero
```
5. Реализация на Python
```python
def compute():
	return str(sum(NUMBERS))[ : 10]
```
#### Вывод 
* Модульная реализация - код лаконичный благодаря функциональным возможностям F# для работы с коллекциями.
* Хвостовая рекурсия - оптимизация при больших данных.
* Рекурсия - привычность, читаемость.


