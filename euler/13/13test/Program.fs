module SumTests

open NUnit.Framework
open FsUnit
open System.Numerics
open Module13

[<TestFixture>]
type ``Sum Functions Tests``() =

    let testNumbers = [
        "12345678901234567890"
        "98765432109876543210"
        "11223344556677889900"
        "99887766554433221100"
        "12345678901234567890"
    ] 

    let testNumbers = testNumbers |> List.map BigInteger.Parse

    let expectedSum = testNumbers |> List.sum

    [<Test>]
    member _.``sumTailRecursion should return the correct sum``() =
        sumTailRecursion testNumbers |> should equal expectedSum

    [<Test>]
    member _.``sumRecursion should return the correct sum``() =
        sumRecursion testNumbers |> should equal expectedSum

    [<Test>]
    member _.``sumWithFold should return the correct sum``() =
        sumWithFold testNumbers |> should equal expectedSum

    [<Test>]
    member _.``sumWithSeq should return the correct sum``() =
        sumWithSeq testNumbers |> should equal expectedSum

    [<Test>]
    member _.``All functions should return the same result``() =
        let sum1 = sumTailRecursion testNumbers
        let sum2 = sumRecursion testNumbers
        let sum3 = sumWithFold testNumbers
        let sum4 = sumWithSeq testNumbers

        sum1 |> should equal sum2
        sum2 |> should equal sum3
        sum3 |> should equal sum4
