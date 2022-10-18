open System.IO
open System.Text.RegularExpressions

type Location = string

type Distance = {
    Destination : Location
    Distance : int
}

type Milestone = {
    Location : Location
    Traveled : int
    Previous : Milestone option
}

let (|Regex|_|) (pattern : string) (input : string) =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parseLine (line : string) =
    match line with
    | Regex @"^([a-zA-Z]+) to ([a-zA-Z]+) = ([0-9]+)$" [a; b; d] ->
        let la = Location(a)
        let lb = Location(b)
        let distance = int(d)
        [|
            (la, { Destination = lb; Distance = distance })
            (lb, { Destination = la; Distance = distance })
        |]
    | _ -> failwithf "Invalid line: %s" line

let parseDistances (file : string) =
    file
    |> File.ReadAllLines
    |> Array.map parseLine
    |> Array.concat
    |> Array.groupBy fst
    |> Array.map (fun (k, v) ->
        k, v |> Array.map snd |> Array.distinct)
    |> Map.ofArray

let adjacent (location : Location) (distances : Map<Location, Distance[]>) =
    distances.[location]

let possibleRoutes (distances : Map<Location, Distance[]>) =
    let validRoute (route : Distance list) =
        if route |> List.length = distances.Keys.Count then
            Some route
        else None

    let calcDistance (route : Distance list) =
        let distance = route |> List.sumBy (fun d -> d.Distance)
        (distance, route)

    let rec loop (travelled : Distance list) (previous : Distance) : Distance list list =
        let next =
            distances
            |> adjacent previous.Destination
            |> Array.filter (fun d ->
                travelled 
                |> List.map (fun t -> t.Destination)
                |> List.contains d.Destination
                |> not)

        let travelled = previous::travelled

        if next |> Array.length = 0 then
            [ travelled |> List.rev ]
        else
            next
            |> Array.Parallel.map (fun p -> loop travelled p)
            |> List.concat

    distances
    |> Map.keys
    |> Seq.map (fun l -> { Destination = l; Distance = 0 })
    |> Array.ofSeq
    |> Array.Parallel.map (fun p -> loop [] p)
    |> Array.toList
    |> List.concat
    |> List.distinct
    |> List.choose validRoute
    |> List.map calcDistance
    |> List.sortBy fst

let part1 file =
    file
    |> parseDistances
    |> possibleRoutes
    |> List.head
    
part1 "test.txt"