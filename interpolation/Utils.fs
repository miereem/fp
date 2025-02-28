module Utils

open Interpolation

let (|IsLinearMethod|_|) (input: string) =
    if input.Equals("linear") then Some Linear else None

let (|IsLagrangeMethod|_|) (input: string) =
    match input.Split("=") with
    | [| "lagrange"; value |] ->
        match Parser.tryParseInt16 value with
        | Some(int) -> Some(Lagrange(int))
        | None -> None
    | _ -> None

let parseArgumentMethod (input: string) =
    match input with
    | IsLinearMethod m -> Ok m
    | IsLagrangeMethod m -> Ok m
    | _ -> Error(sprintf "Unknown arg %s input" input)

let readLines =
    Seq.initInfinite (fun _ ->
        try
            System.Console.ReadLine()
        with ex ->
            null)
    |> Seq.takeWhile (not << isNull)