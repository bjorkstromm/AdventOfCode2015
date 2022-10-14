open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

type Wire =
| Id of string
| Val of uint16

type Gate =
| AND    of Left: Wire * Right: Wire
| OR     of Left: Wire * Right: Wire
| LSHIFT of Left: Wire * Right: uint16
| RSHIFT of Left: Wire * Right: uint16
| NOT    of Left: Wire
| INPUT  of Left: Wire

// Example
// 123 -> x
// 456 -> y
// x AND y -> d
// x OR y -> e
// x LSHIFT 2 -> f
// y RSHIFT 2 -> g
// NOT x -> h
// NOT y -> i

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parseLine (str : string) =
    match str with
    | Regex @"^([0-9]+) -> ([a-z]+)$" [num;id] -> (Id(id), INPUT(Val(UInt16.Parse(num))))
    | Regex @"^([a-z]+) -> ([a-z]+)$" [signal;id] -> (Id(id), INPUT(Id(signal)))
    | Regex @"^NOT ([a-z]+) -> ([a-z]+)$" [signal; id] -> (Id(id), NOT(Id(signal)))
    | Regex @"^([0-9]+) AND ([a-z]+) -> ([a-z]+)$" [num; right; id] -> (Id(id), AND(Val(UInt16.Parse(num)), Id(right)))
    | Regex @"^([a-z]+) AND ([a-z]+) -> ([a-z]+)$" [left; right; id] -> (Id(id), AND(Id(left), Id(right)))
    | Regex @"^([a-z]+) OR ([a-z]+) -> ([a-z]+)$" [left; right; id] -> (Id(id), OR(Id(left), Id(right)))
    | Regex @"^([a-z]+) LSHIFT ([0-9]+) -> ([a-z]+)$" [left; right; id] -> (Id(id), LSHIFT(Id(left), UInt16.Parse(right)))
    | Regex @"^([a-z]+) RSHIFT ([0-9]+) -> ([a-z]+)$" [left; right; id] -> (Id(id), RSHIFT(Id(left), UInt16.Parse(right)))
    | _ -> failwithf "Invalid line: %s" str

let parseLines (lines : seq<string>) =
    lines
    |> Seq.map parseLine
    |> Map.ofSeq

let parseFile file = 
    file
    |> File.ReadAllLines
    |> parseLines

let rec eval (signals : Map<Wire, Gate>) (signal: Wire) (cache: Dictionary<string, uint16>) =
    printfn "eval: %A" signal

    match signal with
    | Val v -> v
    | Id id when cache.ContainsKey id ->
        printfn "cache hit: %s" id
        cache.[id]
    | Id id ->
        let v =
            match signals.[signal] with
            | INPUT signal ->
                match signal with
                | Val v -> v
                | Id id -> eval signals (Id(id)) cache
            | NOT signal -> ~~~ (eval signals signal cache)
            | AND (left ,right) -> (eval signals left cache) &&& (eval signals right cache)
            | OR (left ,right) -> (eval signals left cache) ||| (eval signals right cache)
            | LSHIFT (left ,right) -> (eval signals left cache) <<< (right |> int)
            | RSHIFT (left ,right) -> (eval signals left cache) >>> (right |> int)
        cache.[id] <- v
        v

let part1 (file : string) (wire : string) =
    let signals = file |> parseFile
    let cache = Dictionary<string, uint16>()
    let result = eval signals (Id(wire)) cache
    result

let part2 (file : string) =
    // Take wire a value
    let signals = file |> parseFile
    let cache = Dictionary<string, uint16>()
    let result = eval signals (Id("a")) cache

    // Override value of wire b
    let signals =
        signals
        |> Map.change (Id("b")) (fun _ -> INPUT(Val(result)) |> Some)
    let cache = Dictionary<string, uint16>()
    let result = eval signals (Id("a")) cache

    result

part2 "day07.txt" |> printfn "%A"