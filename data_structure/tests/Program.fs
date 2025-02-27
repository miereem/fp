module MultiSetPropertyTests

open NUnit.Framework
open FsCheck
open Multiset
open System
open System.IO

// Helper function to compare two MultiSets for equality
let areMultiSetsEqual (set1: MultiSet<'T>) (set2: MultiSet<'T>) =
    if set1.Size <> set2.Size then
        false
    else
        set1.Table
        |> Array.forall2 (fun x y ->
            match x, y with
            | Some (_, e1, c1), Some (_, e2, c2) -> e1 = e2 && c1 = c2
            | None, None -> true
            | _ -> false) set2.Table

// Generator for creating random MultiSets
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

// Property test: Adding an element increases its count
[<Test>]
let ``Add Property - Adding Element Increases Count``() =
    let property (set: MultiSet<int>) =
        let element = 54
        let newSet = add element 1 set
        count element newSet = count element set + 1

    Prop.forAll testMultiSet property
    |> Check.QuickThrowOnFailure

// Property test: Adding zero or negative count does not change the set
[<Test>]
let ``Add Property - Adding Zero or Negative Count Does Not Change Set``() =
    let property (set: MultiSet<int>) =
        let element = 56
        let newSet = add element 0 set
        areMultiSetsEqual set newSet

    Prop.forAll testMultiSet property
    |> Check.QuickThrowOnFailure

// Property test: Count of an element not in the set is zero
[<Test>]
let ``Count Property - Count of Missing Element is Zero``() =
    let property (set: MultiSet<int>) =
        let element = 89
        not (set.Table |> Array.exists (function
            | Some (_, e, _) -> e = element
            | None -> false))
        ==> lazy (count element set = 0)

    Prop.forAll testMultiSet property
    |> Check.QuickThrowOnFailure

// Property test: Union with an empty set returns the original set
[<Test>]
let ``Union Property - Union with Empty Set Returns Original Set``() =
    let property (set: MultiSet<int>) =
        let emptySet = empty 16
        let unionSet = union set emptySet
        areMultiSetsEqual set unionSet

    Prop.forAll testMultiSet property
    |> Check.QuickThrowOnFailure

// Property test: Union is associative
[<Test>]
let ``Associative Property - Union is Associative``() =
    let property
        (
            set1: MultiSet<int>,
            set2: MultiSet<int>,
            set3: MultiSet<int>
        ) =
        let unionLeft = union (union set1 set2) set3
        let unionRight = union set1 (union set2 set3)
        areMultiSetsEqual unionLeft unionRight

    let testTripleSet = Arb.fromGen (Gen.zip3 generateMultiSet generateMultiSet generateMultiSet)

    Prop.forAll testTripleSet property
    |> Check.QuickThrowOnFailure

// Property test: Empty set is empty
[<Test>]
let ``IsEmpty Property - Empty Set is Empty``() =
    let property () =
        let emptySet = empty 16
        isEmpty emptySet

    Check.QuickThrowOnFailure property

// Property test: Non-empty set is not empty
[<Test>]
let ``IsEmpty Property - Non-Empty Set is Not Empty``() =
    let property (element: int) =
        let set = add element 1 (empty 16)
        not (isEmpty set)

    Check.QuickThrowOnFailure property

// SetUp method to prepare the environment before running tests
[<OneTimeSetUp>]
let initialize() =
    let stringWriter = new StringWriter()
    System.Console.SetOut(stringWriter)