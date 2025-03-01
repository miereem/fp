module MultiSetTests

open NUnit.Framework
open Multiset
open NUnit.Framework
open FsUnit

[<Test>]
let ``Empty MultiSet should be empty`` () =
    let ms = empty 10
    Assert.IsTrue(isEmpty ms)

[<Test>]
let ``Adding an element should increase its count`` () =
    let ms = empty 10 |> add "apple" 2
    Assert.AreEqual(2, count "apple" ms)

[<Test>]
let ``Adding zero or negative count should not change the MultiSet`` () =
    let ms = empty 10 |> add "banana" 0
    Assert.AreEqual(0, count "banana" ms)
    let ms2 = ms |> add "banana" -3
    Assert.AreEqual(0, count "banana" ms2)

[<Test>]
let ``Adding the same element multiple times should accumulate counts`` () =
    let ms = empty 10 |> add "cherry" 2 |> add "cherry" 3
    Assert.AreEqual(5, count "cherry" ms)

[<Test>]
let ``Union of two MultiSets should merge counts`` () =
    let ms1 = empty 10 |> add "dog" 3
    let ms2 = empty 10 |> add "dog" 2 |> add "cat" 1
    let result = union ms1 ms2
    Assert.AreEqual(5, count "dog" result)
    Assert.AreEqual(1, count "cat" result)

[<Test>]
let ``Union with empty MultiSet should not change`` () =
    let ms = empty 10 |> add "elephant" 4
    let result = union ms (empty 10)
    Assert.AreEqual(4, count "elephant" result)

[<Test>]
let ``filter should keep elements that satisfy the predicate`` () =
    let set = empty 10 |> add "apple" 2 |> add "banana" 1 |> add "cherry" 3
    let predicate = fun e -> e = "apple"

    let filteredSet = filter predicate set

    count "apple" filteredSet |> should equal 2
    count "cherry" filteredSet |> should equal 0
    count "banana" filteredSet |> should equal 0

[<Test>]
let ``filter should return an empty MultiSet if no elements satisfy the predicate`` () =
    let set = empty 10 |> add "apple" 2 |> add "banana" 1
    let predicate = fun e -> e = "cherry" 

    let filteredSet = filter predicate set

    isEmpty filteredSet |> should be True

[<Test>]
let ``foldLeft should sum the counts of all elements`` () =
    let set = empty 10 |> add "apple" 2 |> add "banana" 1 |> add "cherry" 3
    let folder acc _ count = acc + count

    let totalCount = foldLeft folder 0 set

    totalCount |> should equal 6 

[<Test>]
let ``foldLeft should return the initial state for an empty MultiSet`` () =
    let set = empty 10
    let folder acc _ count = acc + count

    let result = foldLeft folder 42 set 

    result |> should equal 42

[<Test>]
let ``foldRight should return the initial state for an empty MultiSet`` () =
    let set = empty 10
    let folder element _ acc = sprintf "%s; %s" element acc

    let result = foldRight folder "initial" set

    result |> should equal "initial"