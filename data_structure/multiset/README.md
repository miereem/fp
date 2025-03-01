# Лабораторная работа №2

Выполнила: Абдурасул кызы Мээрим, P34092

Вариант: oa-bag

Интерфейс: Bag

Структура данных: OpenAddress Multiset

## Задание

Цель: освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing, property-based testing), а также разделением интерфейса и особенностей реализации.

### Требования
1. Функции:
    - добавление и удаление элементов;
    - фильтрация;
    - отображение (map);
    - свертки (левая и правая);
    - структура должна быть моноидом.
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.
7. Обратите внимание:
    - API должно быть реализовано для заданного интерфейса и оно не должно "протекать". На уровне тестов -- в первую очередь нужно протестировать именно API (dict, set, bag).
    - Должна быть эффективная реализация функции сравнения (не наивное приведение к спискам, их сортировка с последующим сравнением), реализованная на уровне API, а не внутреннего представления.

## Реализация

### Функция поиска hash 

Входные аргументы:
- Вместимость (максимальный размер) структуры
- Ключ

Выходные аргументы:
- hash

```
let calculateHash (maxSize: int) (key: 'Key) : int =
    (hash key) % maxSize
    |> (fun x -> if x < 0 then x + maxSize else x)
```

### Функция "Поиск ячейки"

```
    (hash + i) % capacity
```

### Функция "Добавление элемента"

```
let add (element : 'T) (count : int) (set : MultiSet<'T>) : MultiSet<'T> =
    if count <= 0 then set
    else
        let hash = abs (element.GetHashCode())
        let rec loop i (set: MultiSet<'T>) =
            let slot = findSlot hash set.Capacity i
            match set.Table.[slot] with
            | None ->
                let newTable = Array.copy set.Table
                newTable.[slot] <- Some (hash, element, count)
                { set with Table = newTable; Size = set.Size + 1 }
            | Some (_, e, c) when e = element ->
                let newTable = Array.copy set.Table
                newTable.[slot] <- Some (hash, element, c + count)
                { set with Table = newTable }
            | _ -> loop (i + 1) set
        loop 0 set
```

### Функция "Удаление элемента"

```
let delete (element : 'T) (set : MultiSet<'T>) : MultiSet<'T> =
    let hash = abs (element.GetHashCode())
    let rec loop i (set: MultiSet<'T>) =
        let slot = findSlot hash set.Capacity i
        match set.Table.[slot] with
        | None -> set 
        | Some (_, e, c) when e = element ->
            if c > 1 then
                let newTable = Array.copy set.Table
                newTable.[slot] <- Some (hash, element, c - 1)
                { set with Table = newTable }
            else
                let newTable = Array.copy set.Table
                newTable.[slot] <- None
                { set with Table = newTable; Size = set.Size - 1 }
        | _ -> loop (i + 1) set
    loop 0 set
```

### Функция "Фильтрация" с предикатом

```
let filter (predicate : 'T -> bool) (set : MultiSet<'T>) : MultiSet<'T> =
    let newTable = Array.create set.Capacity None
    let mutable newSize = 0
    
    for i in 0 .. set.Capacity - 1 do
        match set.Table.[i] with
        | Some (hash, e, c) when predicate e ->
            let rec insert j =
                let slot = findSlot hash set.Capacity j
                match newTable.[slot] with
                | None ->
                    newTable.[slot] <- Some (hash, e, c)
                    newSize <- newSize + 1
                | _ -> insert (j + 1)
            insert 0
        | _ -> ()
    
    { Table = newTable; Capacity = set.Capacity; Size = newSize }

```

### Функция "Свертки"

```
let foldLeft (folder : 'State -> 'T -> int -> 'State) (state : 'State) (set : MultiSet<'T>) : 'State =
    set.Table
    |> Array.fold (fun acc entry ->
        match entry with
        | Some (_, e, c) -> folder acc e c
        | None -> acc) state

let foldRight (folder : 'T -> int -> 'State -> 'State) (state : 'State) (set : MultiSet<'T>) : 'State =
    set.Table
    |> Array.choose (fun entry ->
        match entry with
        | Some (_, e, c) -> Some (e, c)
        | None -> None)
    |> Array.rev
    |> Array.fold (fun acc (e, c) -> folder e c acc) state
```



### Моноидо

1. Нейтральный элемент


```
let zero<'T when 'T : equality> : MultiSet<'T> = empty 0
```


2. Бинарная операция

В качестве бинарной операции используем union. Она представлена ниже.

```
let union (set1 : MultiSet<'T>) (set2 : MultiSet<'T>) : MultiSet<'T> =
    if isEmpty set1 then set2
    elif isEmpty set2 then set1
    else
        let combinedCapacity = set1.Capacity + set2.Capacity
        let result = empty combinedCapacity

        let resultWithSet1 =
            set1.Table
            |> Array.fold (fun acc entry ->
                match entry with
                | Some (_, e, c) -> add e c acc
                | None -> acc) result

        set2.Table
        |> Array.fold (fun acc entry ->
            match entry with
            | Some (_, e, c) -> add e c acc
            | None -> acc) resultWithSet1
```

## Демонстрационная программа

```
let main _ =
    let set1 = empty 16
    printfn "Создали пустое мультимножество"

    let set1 = set1 |> add "apple" 2 |> add "banana" 3 |> add "apple" 1
    printfn "Добавили элементы: 'apple' 3 раза и 'banana' 3 раза"

    printfn "'apple' встречается %d раз" (count "apple" set1)
    printfn "'banana' встречается %d раз" (count "banana" set1)
    printfn "'orange' встречается %d раз" (count "orange" set1)

    printfn "Множество пустое? %b" (isEmpty set1)

    let set2 = empty 16 |> add "apple" 1 |> add "cherry" 5
    printfn "Создали второе множество и добавили 'apple' 1 раз и 'cherry' 5 раз"

    let unionSet = union set1 set2
    printfn "Объединили множества"

    printfn "'apple' встречается %d раз в объединенном множестве" (count "apple" unionSet)
    printfn "'cherry' встречается %d раз в объединенном множестве" (count "cherry" unionSet)
    printfn "'banana' встречается %d раз в объединенном множестве" (count "banana" unionSet)

    printfn "Тестирование завершено"
    0
```

## Тестирование

Всего было разработано 17 тестов:
PropertyBased: 1) Union is Associative
               2) Empty Set
               3) Adding element increases count
               4) Non-empty set is not empty

Результат тестирования:
```
Passed!  - Failed:     0, Passed:    17, Skipped:     0, Total:    17, Duration: 88 ms - tests.dll (net8.0)
```

Для тестирования использовались модули: 
- Nunit/Xunit 
- FsCheck 

## Заключение
Использование линейного пробирования дало простую и эффективную реализацию мультимножества, а сочетание NUnit и FsCheck обеспечило надежное тестирование

Тестирование MultiSet
Unit-тестирование 
Проверяются базовые операции: добавление, удаление, объединение, фильтрация.
Тестируются граничные случаи, например, работа с пустым множеством.

Property-тестирование: Генерируются случайные множества и проверяются инварианты

