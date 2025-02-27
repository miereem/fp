module MultiSetTests

open NUnit.Framework
open FsCheck
open System
open Multiset

// Генератор для MultiSet
let multiSetGenerator<'T when 'T : equality> : Gen<MultiSet<'T>> =
    Gen.listOf Arb.generate<'T>
    |> Gen.map (fun elements ->
        let set = empty 16
        elements |> List.fold (fun s e -> add e 1 s) set)

// Регистрация генератора
type MyGenerators =
    static member MultiSet() =
        { new Arbitrary<MultiSet<int>>() with
            override _.Generator = multiSetGenerator<int> }

// Свойство: Добавление элемента увеличивает его количество
[<Test>]
let ``Adding an element increases its count`` () =
    let property (element : int) (set : MultiSet<int>) =
        let newSet = add element 1 set
        count element newSet = count element set + 1
    Check.QuickThrowOnFailure property

// Свойство: Добавление элемента не влияет на количество других элементов
[<Test>]
let ``Adding an element does not affect other elements`` () =
    let property (element1 : int) (element2 : int) (set : MultiSet<int>) =
        element1 <> element2 ==> lazy (
            let newSet = add element1 1 set
            count element2 newSet = count element2 set
        )
    Check.QuickThrowOnFailure property

// Свойство: Объединение двух множеств содержит все элементы из обоих
[<Test>]
let ``Union of two sets contains all elements from both`` () =
    let property (set1 : MultiSet<int>) (set2 : MultiSet<int>) =
        let unionSet = union set1 set2

        let allElements = 
            (set1 |> toList) @ (set2 |> toList)
            |> List.distinct

        allElements |> List.forall (fun e ->
            count e unionSet = count e set1 + count e set2
        )

    Check.QuickThrowOnFailure property

// Свойство: Пустое множество действительно пустое
[<Test>]
let ``Empty set is empty`` () =
    let property () =
        let set = empty 16
        isEmpty set
    Check.QuickThrowOnFailure property