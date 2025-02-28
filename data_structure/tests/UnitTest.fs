module MultiSetTests

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Multiset

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

