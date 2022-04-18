open System.IO

type Box = {
    l: int
    w: int
    h: int
}

let calculateSurfaceAreas box =
    (box.l * box.w), (box.w * box.h), (box.h * box.l)

let calculateWrappingPaper box =
    let (a,b,c) = calculateSurfaceAreas box
    let slack = min a (min b c)

    (2*a) + (2*b) + (2*c) + slack

let calculateTotalWrappingPaper boxes =
    boxes
    |> Seq.sumBy calculateWrappingPaper

let parseBox (exp : string) =
    match exp.Split('x') with
    | [|l;w;h|] -> { l = int l; w = int w; h = int h }
    | _ -> failwithf "Invalid box expression %s" exp

let readInput filename =
    filename
    |> File.ReadAllLines

// Part 1
let part1 filename =
    filename
    |> readInput
    |> Seq.map parseBox
    |> calculateTotalWrappingPaper

// Part 2
let calculateRibbon box =
    let smallest =
        [|box.l;box.w;box.h|]
        |> Array.sort
        |> Array.take 2
    let ribbon = (smallest.[0] * 2) + (smallest.[1] * 2)
    let bow = box.l * box.w * box.h

    ribbon + bow

let calculateTotalRibbon boxes =
    boxes
    |> Seq.sumBy calculateRibbon

let part2 filename =
    filename
    |> readInput
    |> Seq.map parseBox
    |> calculateTotalRibbon