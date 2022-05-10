open System.Text.RegularExpressions

type State =
    | Off
    | On

type Action =
    | TurnOn
    | TurnOff
    | Toggle

type Coordinate = {
    X: int
    Y: int
}

type Instruction = {
    Action: Action
    Start: Coordinate
    Stop: Coordinate
}

let emptyGrid size : State[,] =
    Array2D.init size size (fun _ _ -> Off)

let (|RegexMatch|_|) exp str =
    let m = Regex.Match(str, exp)
    if m.Success then
        m.Groups
        |> Seq.skip 1
        |> Seq.map (fun g -> g.Value)
        |> Seq.toList
        |> Some
    else
        None

let parseInstruction str =
    let parseCoordinate (str : string) =
        match str.Split(',') with
        | [|x;y|] -> { X = int x; Y = int y }
        | _ -> failwithf "Invalid coordinate %s" str
    match str with
    | RegexMatch @"turn\son\s(\d+,\d+)\sthrough\s(\d+,\d+)" [s;e]
        -> { Action = TurnOn; Start = parseCoordinate s; Stop = parseCoordinate e }
    | RegexMatch @"turn\soff\s(\d+,\d+)\sthrough\s(\d+,\d+)" [s;e]
        -> { Action = TurnOff; Start = parseCoordinate s; Stop = parseCoordinate e }
    | RegexMatch @"toggle\s(\d+,\d+)\sthrough\s(\d+,\d+)" [s;e]
        -> { Action = Toggle; Start = parseCoordinate s; Stop = parseCoordinate e }
    | _ -> failwithf "Invalid instruction %s" str

let readLines filename =
    filename |> System.IO.File.ReadAllLines

let eval (grid : State[,]) instruction =
    for i in instruction.Start.X..instruction.Stop.X do
        for j in instruction.Start.Y..instruction.Stop.Y do
            match instruction.Action with
            | TurnOn -> grid[i,j] <- On
            | TurnOff -> grid[i,j] <- Off
            | Toggle -> grid[i,j] <- if grid[i,j] = On then Off else On
    grid

let part1 filename =
    filename
    |> readLines
    |> Array.map parseInstruction
    |> Array.fold eval (emptyGrid 1000)
    |> Seq.cast<State>
    |> Seq.filter (fun x -> x = On)
    |> Seq.length

// Part 2

let emptyGrid2 size : int[,] =
    Array2D.zeroCreate size size

let eval2 (grid : int[,]) instruction =
    for i in instruction.Start.X..instruction.Stop.X do
        for j in instruction.Start.Y..instruction.Stop.Y do
            match instruction.Action with
            | TurnOn -> grid[i,j] <- grid[i,j] + 1
            | TurnOff -> grid[i,j] <- max (grid[i,j] - 1) 0
            | Toggle -> grid[i,j] <- grid[i,j] + 2
    grid

let part2 filename =
    filename
    |> readLines
    |> Array.map parseInstruction
    |> Array.fold eval2 (emptyGrid2 1000)
    |> Seq.cast<int>
    |> Seq.sum