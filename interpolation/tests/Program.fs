module Tests

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
