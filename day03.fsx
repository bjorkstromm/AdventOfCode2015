open System.IO

type Direction =
    | North
    | South
    | East
    | West

type Position = {
    x: int
    y: int
}

let parseInstructions str =
    str
    |> Seq.map (fun c -> 
        match c with
        | '^' -> North
        | 'v' -> South
        | '>' -> East
        | '<' -> West
        | c -> failwithf "Invalid instruction %c" c)

let readInstructions filename =
    File.ReadAllText filename
    |> parseInstructions 

let processInstructions instructions =
    let folder (pos, map) instruction =
        let pos =
            match instruction with
            | North -> { x = pos.x; y = pos.y + 1 }
            | South -> { x = pos.x; y = pos.y - 1 }
            | East -> { x = pos.x + 1; y = pos.y }
            | West -> { x = pos.x - 1; y = pos.y }

        let map =
            match map |> Map.tryFind pos with
            | Some n -> map |> Map.add pos (n + 1)
            | None -> map |> Map.add pos 1

        (pos, map)

    let pos = { x = 0; y = 0 }
    let map = 
        seq {(pos, 1)}
        |> Map.ofSeq

    instructions
    |> Seq.fold folder (pos, map)
    |> snd

// Part 1
let part1 filename =
    filename
    |> readInstructions
    |> processInstructions
    |> Map.toSeq
    |> Seq.length

// Part 2
let processInstructionsWithRoboSanta instructions =
    let advance pos map instruction =
        let pos =
            match instruction with
            | North -> { x = pos.x; y = pos.y + 1 }
            | South -> { x = pos.x; y = pos.y - 1 }
            | East -> { x = pos.x + 1; y = pos.y }
            | West -> { x = pos.x - 1; y = pos.y }

        let map =
            match map |> Map.tryFind pos with
            | Some n -> map |> Map.add pos (n + 1)
            | None -> map |> Map.add pos 1

        (pos, map)

    let folder ((santaPos, roboPos), map) (instruction : Direction[]) =
        let (santaPos, map) = advance santaPos map (instruction.[0])
        let (roboPos, map) = advance roboPos map (instruction.[1])

        ((santaPos, roboPos), map)

    let pos = { x = 0; y = 0 }
    let map = 
        seq {(pos, 2)}
        |> Map.ofSeq

    instructions
    |> Seq.chunkBySize 2
    |> Seq.fold folder ((pos, pos), map)
    |> snd

let part2 filename =
    filename
    |> readInstructions
    |> processInstructionsWithRoboSanta
    |> Map.toSeq
    |> Seq.length