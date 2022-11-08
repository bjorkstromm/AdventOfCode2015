open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parseLine (str : string) =
    match str with
    | Regex @"^([A-Za-z]+) would gain ([0-9]+) happiness units by sitting next to ([A-Za-z]+).$" [name;units;adjacent] ->
        (name, (adjacent, (units |> int)))
    | Regex @"^([A-Za-z]+) would lose ([0-9]+) happiness units by sitting next to ([A-Za-z]+).$" [name;units;adjacent] ->
        (name, (adjacent, (units |> int |> (*) -1)))
    | _ -> failwithf "Invalid line: %s" str

let getRules filename =
    filename
    |> File.ReadAllLines
    |> Seq.map parseLine
    |> Seq.groupBy fst
    |> Seq.map (fun (x, xs) -> (x, xs |> Seq.map snd |> Map.ofSeq))
    |> Map.ofSeq

let distribute e lst =
    let rec loop pre post = 
        seq {
            match post with
            | [] -> yield (lst @ [e])
            | head::tail ->
                yield (List.rev pre @ [e] @ post)
                yield! loop (head::pre) tail
        }
    loop [] lst

let rec permute lst =
    match lst with
    | [] -> Seq.singleton []
    | head::tail -> Seq.collect (distribute head) (permute tail)

let calcHappiness (rules : Map<string, Map<string, int>>) seating =
    let happiness i =
        let happ r x =
            match r |> Map.tryFind x with
            | Some i -> i
            | _ -> 0

        let li = (if i = 0 then List.length seating else i) - 1
        let ri = if i + 1 = List.length seating then 0 else i + 1
        let l = seating.[li]
        let r = seating.[ri]

        let rule = rules.[seating.[i]]
        happ rule l + happ rule r

    seating
    |> List.mapi (fun i _ -> happiness i)
    |> List.sum


let part1 filename =
    let rules = getRules filename
    let first = rules.Keys |> Seq.head
    let optimal =
        rules.Keys
        |> List.ofSeq
        |> permute
        |> Seq.filter (fun xs -> xs |> List.head = first)
        |> Seq.map (calcHappiness rules)
        |> Seq.sortDescending
        |> Seq.head

    optimal

let part2 filename = 
    let rules =
        filename
        |> getRules
        |> Map.add "Me" Map.empty
    let first = rules.Keys |> Seq.head
    let optimal =
        rules.Keys
        |> List.ofSeq
        |> permute
        |> Seq.filter (fun xs -> xs |> List.head = first)
        |> Seq.map (calcHappiness rules)
        |> Seq.sortDescending
        |> Seq.head

    optimal

"test.txt" |> part2
"day13.txt" |> part2