open System.IO

type Direction =
    | Up
    | Down

let parseInstructions str =
    str
    |> Seq.map (fun c -> 
        match c with
        | '(' -> Up
        | ')' -> Down
        | c -> failwithf "Invalid instruction %c" c)

let readInstructions filename =
    File.ReadAllText filename
    |> parseInstructions 

let processInstructions instructions =
    let folder state instruction =
        match instruction with
        | Up -> state + 1
        | Down -> state - 1

    instructions
    |> Seq.fold folder 0

// Part 1
let part1 filename =
    filename
    |> readInstructions
    |> processInstructions

// Part 2
let processInstructionsUntilBasement instructions =
    let rec loop position index instructions =
        match instructions with
        | [] -> failwith "Basement never reached"
        | instruction::tail ->
            let position =
                match instruction with
                | Up -> position + 1
                | Down -> position - 1
            if position < 0 then
                index
            else
                tail |> loop position (index + 1)

    instructions
    |> Seq.toList
    |> loop 0 1

let part2 filename =
    filename
    |> readInstructions
    |> processInstructionsUntilBasement