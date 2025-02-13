module Main
open System
open Program

[<EntryPoint>]
let main _ =
    // Создаем пустое мультимножество
    let set1 = empty 16
    printfn "Создали пустое мультимножество"

    // Добавляем элементы
    let set1 = set1 |> add "apple" 2 |> add "banana" 3 |> add "apple" 1
    printfn "Добавили элементы: 'apple' 3 раза и 'banana' 3 раза"

    // Проверяем количество вхождений
    printfn "'apple' встречается %d раз" (count "apple" set1)
    printfn "'banana' встречается %d раз" (count "banana" set1)
    printfn "'orange' встречается %d раз" (count "orange" set1)

    // Проверяем, пустое ли множество
    printfn "Множество пустое? %b" (isEmpty set1)

    // Создаем второе множество
    let set2 = empty 16 |> add "apple" 1 |> add "cherry" 5
    printfn "Создали второе множество и добавили 'apple' 1 раз и 'cherry' 5 раз"

    // Объединяем множества
    let unionSet = union set1 set2
    printfn "Объединили множества"

    // Проверяем количество в объединенном множестве
    printfn "'apple' встречается %d раз в объединенном множестве" (count "apple" unionSet)
    printfn "'cherry' встречается %d раз в объединенном множестве" (count "cherry" unionSet)
    printfn "'banana' встречается %d раз в объединенном множестве" (count "banana" unionSet)

    // Завершение
    printfn "Тестирование завершено"
    0
