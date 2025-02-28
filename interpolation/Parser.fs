module Parser

// Попытка преобразования строки в тип float
let tryParseFloat (input: string) =
    // Используется метод TryParse для безопасного преобразования строки в число с плавающей точкой (float).
    // Метод TryParse возвращает кортеж: первый элемент - успех преобразования (bool), второй - преобразованное значение (если удалось).
    match System.Double.TryParse(
            input,  // Строка, которую нужно преобразовать.
            System.Globalization.NumberStyles.Number,  // Указываем, что строка может содержать числа с плавающей точкой (например, "3.14").
            System.Globalization.CultureInfo.InvariantCulture  // Используем инвариантную культуру, чтобы избежать проблем с различиями в разделителях (например, точки или запятые).
        ) with
    | true, value -> Some value
    | _ -> None

// Попытка преобразования строки в тип int16 (целое число 16 бит)
let tryParseInt16 (input: string) =
    // Аналогично tryParseFloat, но пытаемся преобразовать строку в целое число (тип Int16).
    match System.Int16.TryParse(
            input,  // Строка, которую нужно преобразовать.
            System.Globalization.NumberStyles.Number,  // Строка должна быть числом.
            System.Globalization.CultureInfo.InvariantCulture  // Опять же, используем инвариантную культуру для единообразия.
        ) with
    | true, value -> Some value  // Если преобразование успешно, возвращаем значение в виде Some.
    | _ -> None  // Если преобразование не удалось, возвращаем None.