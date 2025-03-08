open Interpolation
open Utils
open Plotly.NET

[<EntryPoint>]
let main (args) =
    let stepArg = Parser.tryParseFloat args[0]

    let methodsArg = args |> Seq.skip 1 |> Seq.map Utils.parseArgumentMethod

    if stepArg.IsSome && methodsArg |> Seq.forall Result.isOk then

        let step = stepArg.Value
        printfn "1. Step: %.2f" step

        let methods = methodsArg |> Seq.choose Result.toOption
        printfn "2. Methods: %A" methods

        let points =
            Utils.readLines
            |> Seq.map Validator.parseAndValidate
            |> Seq.choose (fun parseResult ->
                match parseResult with
                | Ok point -> Some point
                | Error errList ->
                    printfn "Errors list:"

                    errList
                    |> Seq.map Validator.errorToString
                    |> Seq.reduce (fun x y -> x + "\n" + y)
                    |> printfn "%s"

                    printfn "Skipping line"
                    None)
            |> Seq.cache

        interpolateStream points methods step
        |> Seq.iter (fun seq ->
            seq
            |> Seq.zip methods
            |> Seq.iter (fun (m, possiblePoints) ->
                match possiblePoints with
                | Some seq ->
                    printfn "%A" m
                    seq |> Seq.map fst |> Seq.iter (printf "%0.2f\t")
                    printfn ""
                    seq |> Seq.map snd |> Seq.iter (printf "%0.2f\t")
                    printfn "\n"
                | None -> ()))

        0
    else
        if stepArg.IsNone then
            printfn "Unexpected step value: %s" args[0]

        methodsArg
        |> Seq.choose (fun r ->
            match r with
            | Ok r -> None
            | Error s -> Some s)
        |> Seq.iter (printfn "%s")

        printfn "Expected: step interpolate [linear|lagrange={int16}]..."
        0
