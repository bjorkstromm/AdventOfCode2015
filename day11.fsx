let containsIncreasingSequence str =
    let isIncreasing (xs : int[]) =
        let low = xs.[0]
        let xs = xs |> Array.map (fun x -> x - low)
        xs = [|0;1;2|]

    str
    |> Seq.map (fun x -> x |> int)
    |> Seq.windowed 3
    |> Seq.exists isIncreasing

let doesNotContainForbiddenChars str =
    let isForbidden = function
        | 'i' | 'o' | 'l' -> true
        | _ -> false

    str
    |> Seq.exists isForbidden
    |> not

let containsTwoDifferentPairs str =
    let isPair (xs : char[]) =
        if xs.[0] = xs.[1] then Some xs.[0] else None

    let len =
        str
        |> Seq.windowed 2
        |> Seq.choose isPair
        |> Seq.countBy id
        |> Seq.length
    len >= 2

let isValidPassword str =
    containsIncreasingSequence str
    && doesNotContainForbiddenChars str
    && containsTwoDifferentPairs str

let fromBase26 str =
    let folder (acc, i) (c : char) =
        let n = ((c |> bigint) - 96I) * (pown 26I i)
        (acc + n, i + 1)

    str
    |> Seq.rev
    |> Seq.fold folder (0I, 0)
    |> fst

let toBase26 n =
    let rec loop acc n =
        if n = 0I then acc
        else
            let c = (((n - 1I) % 26I) + 97I) |> int |> char
            let n = (n - 1I) / 26I
            loop (c :: acc) n

    n
    |> loop []
    |> List.toArray
    |> System.String

let increment str =
    str
    |> fromBase26
    |> (+) 1I
    |> toBase26

let rec findNextPassword str =
    let str = increment str
    if isValidPassword str then str
    else findNextPassword str