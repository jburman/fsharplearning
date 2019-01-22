open System
open System.IO

[<EntryPoint>]
let main argv =
    let quotedValueParser = CSVParser.readAllLines ',' true

    use reader = File.OpenText("sample.csv")
    let csvLines = quotedValueParser reader
    let header = Seq.tryHead csvLines
    printfn "HEADER %A" header
    printfn "VALUE ROW: %A" (csvLines |> Seq.take 1)
    0
