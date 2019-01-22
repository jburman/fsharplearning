module CSVParser
  
open System.IO 

// counts the occurrences of a character from the specified index backwards  
let rec private countAtEnd (countChar: char) (index: int) (line: string) = 
    if line.Length = 0 then 
        0 
    else 
        if index >= 0 && (line.[index] = countChar) then 
            1 + countAtEnd countChar (index - 1) line 
        else 
            0 

// Checks if a character is escaped with backslashes starting at the specified index
let private isEscapedAtIndex (index: int) (line: string) = 
    if line.Length = 0 || index >= line.Length then 
        false 
    else 
        let count = countAtEnd '\\' index line 
        match count % 2 with 
        | 1 -> true 
        | _ -> false 
  
// Returns a string between a starting index and a specified (unescaped) char. 
// Returns the index 
let rec private readToNonEscaped (searchChar: char) (index: int) (acc: string) (line: string) = 
    if line.Length = index then 
        (index, acc) 
    else 
        let nextChar = line.[index] 
        match nextChar with 
        | c when c = searchChar && not (isEscapedAtIndex (acc.Length - 1) acc) -> (index, acc) 
        | _ -> line |> readToNonEscaped searchChar (index + 1) (acc + nextChar.ToString())

// Attempt to read the next value up to a specified separator character  
let private readNextValue (separator: char) (index: int) (line: string) = 
    if index < line.Length then 
        Some (line |> readToNonEscaped separator index System.String.Empty) 
    else 
        None 

// Read as a list all of the values from a string starting at the specified index and using the specified separator.
let rec private readLineValues (separator: char) (index: int) (line: string) = 
    let value = line |> readNextValue separator index
    match value with 
    | None -> [] 
    | _ ->  
        let (index, fieldVal) = Option.get(value) 
        fieldVal :: (line |> readLineValues separator (index + 1))
  
// Finds (non escaped) quotes wrapping a value and returns everything between them.
let private stripQuotes (value: string) = 
    let firstIndex = value.IndexOf('"') 
    match firstIndex with 
    | -1 -> value 
    | _ when (value.Length - 1) = firstIndex -> value 
    | _ ->  
        let lastIndex = value.LastIndexOf('"') 
        match lastIndex with 
        | -1 -> value 
        | _ when isEscapedAtIndex (lastIndex - 1) value -> value 
        | _ -> value.Substring(firstIndex + 1, lastIndex - firstIndex - 1) 

// Read the list of values from a line and strip quotes if needed
let private parseLine (separator: char) (isQuoted: bool) (line: string) = 
    let lineValues = line |> readLineValues separator 0
    if isQuoted then 
        lineValues |> List.map (fun v -> stripQuotes v) 
    else 
        lineValues 
  
let private isEOF (reader: TextReader) = 
    reader.Peek() = -1 

// Read and parse all the lines from a CSV file and return them as a Seq  
let readAllLines (separator: char) (isQuoted: bool) (reader: TextReader) = 
    let parseLineWithSeparator = parseLine separator isQuoted
    seq {
        while not (isEOF reader) do 
            yield reader.ReadLine() |> parseLineWithSeparator
    } 