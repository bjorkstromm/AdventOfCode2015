open System.IO
open System.Text.RegularExpressions

type Reindeer = {
    Name : string
    Speed : int
    FlyTime : int
    RestTime : int
}

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parseLine (str : string) =
    match str with
    | Regex @"^([A-Za-z]+) can fly ([0-9]+) km/s for ([0-9]+) seconds, but then must rest for ([0-9]+) seconds.$"
            [name;speed;flyTime;restTime] ->
        {
            Name = name
            Speed = speed |> int
            FlyTime = flyTime |> int
            RestTime = restTime |> int
        }
    | _ -> failwithf "Invalid line: %s" str

let parseInput filename =
    filename
    |> File.ReadAllLines
    |> Array.map parseLine

let fly duration reindeer =
    let rec loop distance time =
        if time >= duration then distance
        else
            let time' = time + reindeer.FlyTime
            let distance' =
                if time' >= duration then (duration - time) * reindeer.Speed
                else reindeer.Speed * reindeer.FlyTime
            loop (distance + distance') (time' + reindeer.RestTime)
    loop 0 0

let part1 duration filename =
    filename
    |> parseInput
    |> Array.Parallel.map (fun r -> (r.Name, fly duration r))
    |> Array.maxBy snd