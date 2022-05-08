open System.Linq

let hasThreeVowels (str : string) =
    let vowels = [|'a';'e';'i';'o';'u'|]
    let count =
        vowels
        |> Seq.sumBy (fun v ->
            str
            |> Seq.filter (fun c -> c = v)
            |> Seq.length)
    count >= 3

let hasTwoConsecutiveChars (str : string) =
    let rec loop prev xs =
        match xs with
        | [] -> false
        | head::_ when head = prev -> true
        | head::tail -> loop head tail

    let head = str.[0]
    str
    |> Seq.toList
    |> List.tail
    |> loop head

let doesNotContainString (xs : seq<string>) (str : string) =
    xs |> Seq.forall (fun x -> not(str.Contains(x)))

let doesNotContainForbiddenSequence (str : string) =
    str |> doesNotContainString [|"ab";"cd";"pq";"xy"|]

let isNiceString str =
    hasThreeVowels str &&
    hasTwoConsecutiveChars str &&
    doesNotContainForbiddenSequence str

let getStrings filename =
    filename |> System.IO.File.ReadAllLines

let part1 filename =
    filename
    |> getStrings
    |> Seq.filter isNiceString
    |> Seq.length

// Part 2
let containsPairOfTwoLettersTwiceWithoutOverlapping (str : string) =
    str
    |> Seq.windowed 2
    |> Seq.mapi (fun i pair -> (pair, i))
    |> Seq.groupBy fst
    |> Seq.map (fun (k, v) -> 
        v |> Seq.map snd |> Seq.toList)
    |> Seq.exists (fun xs -> 
        match xs with
        | [x] -> false
        | [x;y] -> abs(y-x) > 1
        | _ -> true)

let containsOneLetterWhichRepeatsWithOneLetterBetween (str : string) =
    str
    |> Seq.windowed 3
    |> Seq.exists (fun window ->
        window[0] = window[2])


let isNiceString2 str =
    containsPairOfTwoLettersTwiceWithoutOverlapping str &&
    containsOneLetterWhichRepeatsWithOneLetterBetween str

let part2 filename =
    filename
    |> getStrings
    |> Seq.filter isNiceString2
    |> Seq.length