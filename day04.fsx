open System.Security.Cryptography;
open System.Text;
open System

let md5 secret num =
    use alg = MD5 .Create()
    
    $"{secret}{num}"
    |> Encoding.ASCII.GetBytes
    |> alg.ComputeHash
    |> BitConverter.ToString
    |> (fun s -> s.Replace("-", ""))

let run secret (start : string) =
    seq { 0 .. Int32.MaxValue }
    |> Seq.takeWhile (
        fun num ->
            let hash = md5 secret num
            not (hash.StartsWith(start)))
    |> Seq.last
    |> (+) 1

let part1 secret =
    run secret "00000"

let part2 secret =
    run secret "000000"