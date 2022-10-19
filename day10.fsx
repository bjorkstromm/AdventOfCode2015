let lookAndSay (str : string) =
    if str.Length = 0 then ""
    else if str.Length = 1 then "1" + str.[0].ToString()
    else
        let loop (n, prev, acc) curr =
            if prev = curr then (n + 1, prev, acc)
            else (1, curr, acc + n.ToString() + prev.ToString())

        let first = str.[0]

        let (n, prev, acc) = 
            str
            |> Seq.skip 1
            |> Seq.fold loop (1, first, "")

        acc + n.ToString() + prev.ToString()


let lookAndSayN (n : int) (str : string) =
    let rec loop (str : string) (n : int) =
        if n = 0 then str
        else loop (lookAndSay str) (n - 1)

    loop str n

let str = "1321131112" |> lookAndSayN 50
str.Length