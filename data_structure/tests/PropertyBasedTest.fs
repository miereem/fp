module MultiSetPropertyTests

open NUnit.Framework
open FsCheck
open Multiset
open System
open System.IO
open FsCheck.Xunit


let generateMultiSet =
    Gen.sized (fun size ->
        gen {
            let! elements = Gen.listOfLength size (Arb.generate<int>)
            let set = empty (size * 2)

            let filledSet =
                List.fold (fun acc e -> add e 1 acc) set elements

            return filledSet
        })

let testMultiSet = Arb.fromGen generateMultiSet

[<Property>]
let ``Add Property - Adding Element Increases Count``(set: MultiSet<int>, element: int) =
        let newSet = add element 1 set
        count element newSet = count element set + 1

[<Property>]
let ``Add Property - Adding Zero or Negative Count Does Not Change Set``(set: MultiSet<int>, element: int, count: int) =
    let newSet = add element count set
    count <= 0 ==> lazy (set.Equals(newSet))

[<Property>]
let ``Count Property - Count of Missing Element is Zero``(set: MultiSet<int>, element: int) =
    not (set.Table |> Array.exists (function
        | Some (_, e, _) -> e = element
        | None -> false))
     ==> lazy (count element set = 0)

[<Test>]
let ``Union Property - Union with Empty Set Returns Original Set``() =
    let property (set: MultiSet<int>) =
        let emptySet = empty 16
        let unionSet = union set emptySet
        set.Equals(unionSet)

    Prop.forAll testMultiSet property
    |> Check.QuickThrowOnFailure

[<Property>]
let ``Union is associative`` (set1 : MultiSet<int>, set2 : MultiSet<int>, set3 : MultiSet<int>) =
    let left = union set1 (union set2 set3) // f(a, f(b, c))
    let right = union (union set1 set2) set3 // f(f(a, b), c)
    left.Equals(right)

[<Test>]
let ``IsEmpty Property - Empty Set is Empty``() =
    let property () =
        let emptySet = empty 16
        isEmpty emptySet

    Check.QuickThrowOnFailure property

[<Test>]
let ``IsEmpty Property - Non-Empty Set is Not Empty``() =
    let property (element: int) =
        let set = add element 1 (empty 16)
        not (isEmpty set)

    Check.QuickThrowOnFailure property

[<OneTimeSetUp>]
let initialize() =
    let stringWriter = new StringWriter()
    System.Console.SetOut(stringWriter)