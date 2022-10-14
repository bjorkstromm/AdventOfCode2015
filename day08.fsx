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

let part1 file =
    file
    |> parseFile
    |> Seq.map calcDiff
    |> Seq.sum

part1 "day08.txt"