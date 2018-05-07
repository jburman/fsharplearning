open System
open System.Globalization
open System.IO
open System.Text

type TokenType =
    | ObjectStart
    | ObjectEnd
    | ArrayStart
    | ArrayEnd
    | Number
    | String
    | True
    | False
    | Null
    | Comma
    | Colon
    | EOF
    | Error

type TokenValue =
    | JSONString of string
    | JSONNumber of decimal

type Token = (TokenType * TokenValue option)

let (|InputToken|Whitespace|) input = if Char.IsWhiteSpace(input) then Whitespace else InputToken

let (|NumberValue|KeywordValue|StringValue|LexicalValue|) input =
    if Char.IsDigit input || '-' = input then NumberValue
    else if Char.IsLetter input then KeywordValue
    else if '"' = input then StringValue
    else LexicalValue

let (|Hexadecimal|_|) (input: char) = if input >= '0' && input <= 'F' then Some(input) else None

let matchKeyword keyword = 
    match keyword with
    | "true" -> TokenType.True
    | "false" -> TokenType.False
    | "null" -> TokenType.Null
    | _ -> failwithf "Invalid keyword found: %s" keyword

let readIgnore (reader: StreamReader) =
    reader.Read() |> ignore

let readNext (reader: StreamReader) =
    let input = char(reader.Read())
    if reader.EndOfStream then
        failwith "Unexpected EOF"
    input

let readHexadecimalChar (reader: StreamReader) =
    let input = readNext reader
    match input with
    | Hexadecimal h -> h
    | _ -> failwithf "Expected hexadecimal value but got %c" input

let readUnicodeSequence (reader: StreamReader) =
    seq {
        yield readHexadecimalChar reader
        yield readHexadecimalChar reader
        yield readHexadecimalChar reader
        yield readHexadecimalChar reader
    } |> Seq.fold (fun hexVal h -> hexVal + h.ToString()) String.Empty

let readEscapeSequence (reader: StreamReader) =
    let input = readNext reader
    match input with
    | '"' -> "\""
    | '\\' -> "\\"
    | '/' -> "/"
    | 'b' -> "\b"
    | 'f' -> "\f"
    | 'n' -> "\n"
    | 'r' -> "\r"
    | 't' -> "\t"
    | 'u' -> Convert.ToInt32(readUnicodeSequence reader, 16) |> Char.ConvertFromUtf32
    | _ -> failwithf "Unexpected escape sequence: \\%c" input

let rec readKeyword (reader: StreamReader) keywordVal =
    let input = char(reader.Peek())
    if Char.IsLetter(input) then
        readNext reader |> ignore
        (input.ToString() + (readKeyword reader keywordVal))
    else
        keywordVal

let rec readString (reader: StreamReader) stringVal =
    let input = readNext reader
    match input with
    | '"' -> stringVal
    | '\\' -> readEscapeSequence reader + (readString reader stringVal)
    | _ -> input.ToString() + (readString reader stringVal)

let rec readNumber (reader: StreamReader) numberVal =
    let input = char(reader.Peek())
    if Char.IsDigit(input) || input = '.' || input = 'E' || input = 'e' || input = '-' then
        readIgnore reader
        input.ToString() + (readNumber reader numberVal)
    else
        numberVal
        
let readToken (reader: StreamReader) (input: char) : Token =
    match input with
    | NumberValue -> 
        TokenType.Number, Some (
            JSONNumber(
                Decimal.Parse(readNumber reader String.Empty, 
                    NumberStyles.AllowDecimalPoint ||| 
                    NumberStyles.AllowExponent ||| 
                    NumberStyles.AllowLeadingSign)))
    | KeywordValue -> readKeyword reader String.Empty |> matchKeyword, None
    | StringValue -> 
        readIgnore reader // read past opening quote
        TokenType.String, Some (JSONString(readString reader String.Empty))
    | LexicalValue ->
        match input with
        | ',' -> readIgnore reader; TokenType.Comma, None
        | ':' -> readIgnore reader; TokenType.Colon, None
        | '{' -> readIgnore reader; TokenType.ObjectStart, None
        | '}' -> readIgnore reader; TokenType.ObjectEnd, None
        | '[' -> readIgnore reader; TokenType.ArrayStart, None
        | ']' -> readIgnore reader; TokenType.ArrayEnd, None
        | _ -> readIgnore reader; TokenType.Error, None

// dotnet run <path-to-json-file>
[<EntryPoint>]
let main argv =
    
    let path = argv.[0]
    let accum = new StringBuilder()

    let tokenize (reader : StreamReader) (accum: StringBuilder) =
        seq {
            while not reader.EndOfStream do
                let input = char(reader.Peek())
                match input with
                | InputToken -> yield readToken reader input;
                | Whitespace -> readIgnore reader
            yield (TokenType.EOF, option.None)
        } : seq<Token>

    use reader = new StreamReader(File.OpenRead(path))
    tokenize reader accum |> Seq.iter (fun t -> printfn "%A" t)

    0
