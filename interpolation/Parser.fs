module Parser

let tryParseFloat (input: string) =
    match System.Double.TryParse(
            input, 
            System.Globalization.NumberStyles.Number, 
            System.Globalization.CultureInfo.InvariantCulture  
        ) with
    | true, value -> Some value
    | _ -> None

let tryParseInt16 (input: string) =
    match System.Int16.TryParse(
            input,  
            System.Globalization.NumberStyles.Number,  
            System.Globalization.CultureInfo.InvariantCulture  
        ) with
    | true, value -> Some value  
    | _ -> None  