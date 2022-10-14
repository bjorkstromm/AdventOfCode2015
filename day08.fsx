open System
open System.IO

type Line = {
    Text : string
    Length : int
    Size : int
}

let calcDiff line = line.Length - line.Size

let calcSize (str : string) =
    let rec loop (size : int) (str : char list) =
        match str with
        | [] -> size
        | char::tail ->
            let skip =
                match char with
                | '\\' -> if tail.[0] = 'x' then 3 else 1
                | _ -> 0
            loop (size + 1) (tail |> List.skip skip)
    str.ToCharArray()
    |> List.ofArray
    |> loop 0
    |> (+) -2 // Remove the quotes

let parseLine (str : string) =
    let length = str.Length
    let size = str |> calcSize
    { Text = str; Length = length; Size = size }

let parseFile file = 
    file
    |> File.ReadAllLines
    |> Seq.map parseLine

// Part 1

let part1 file =
    file
    |> parseFile
    |> Seq.map calcDiff
    |> Seq.sum

part1 "day08.txt"

// Part 2

type LineExt = {
    Original : Line
    Encoded : Line
}

let encode (str : string) =
    let rec loop (acc : string) (str : char list) =
        match str with
        | [] -> acc
        | char::tail ->
            let (add, skip) =
                match char with
                | '\\' ->   if tail.[0] = 'x' then
                                (@"\\" + String(tail.[0..2] |> List.toArray), 3)
                            else
                                (@"\\", 0)
                | '"' -> (@"\""", 0)
                | _ -> (char.ToString(), 0)
            let acc = acc + add
            loop acc (tail |> List.skip skip)
    str.ToCharArray()
    |> List.ofArray
    |> loop ""
    |> sprintf "\"%s\"" // Add the quotes

let encodeLine line =
    let encoded = line.Text |> encode
    let size = encoded |> calcSize
    { Original = line; Encoded = { Text = encoded; Length = encoded.Length; Size = size } }

let part2 file = 
    file
    |> parseFile
    |> Seq.map encodeLine
    |> Seq.sumBy (fun l -> l.Encoded.Length - l.Original.Length)

part2 "day08.txt"