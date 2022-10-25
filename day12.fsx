#r "nuget: Newtonsoft.Json, 13.0.1"

open Newtonsoft.Json.Linq

let parse str =
    JToken.Parse str

let parseFile file =
    file
    |> System.IO.File.ReadAllText
    |> parse

let rec sum (token : JToken) =
    match token.Type with
    | JTokenType.Array ->
        token.Children ()
        |> Seq.sumBy sum
    | JTokenType.Object ->
        token.Children<JProperty> ()
        |> Seq.map (fun p -> p.Value)
        |> Seq.sumBy sum
    | JTokenType.Integer ->
        token.Value<int> ()
    | _ -> 0

let rec sum2 (token : JToken) =
    let hasPropertyWithValueRed (token : JToken) =
        token.Children<JProperty> ()
        |> Seq.exists (fun p ->
            p.Value.Type = JTokenType.String
            && p.Value.Value<string> () = "red")

    match token.Type with
    | JTokenType.Array ->
        token.Children ()
        |> Seq.sumBy sum2
    | JTokenType.Object ->
        if hasPropertyWithValueRed token then 0
        else
            token.Children<JProperty> ()
            |> Seq.map (fun p -> p.Value)
            |> Seq.sumBy sum2
    | JTokenType.Integer ->
        token.Value<int> ()
    | _ -> 0

let part1 file =
    file
    |> parseFile
    |> sum

let part2 file =
    file
    |> parseFile
    |> sum2

"day12.txt" |> part1

"[1,2,3]" |> parse |> sum 
"{\"a\":2,\"b\":4}" |> parse |> sum
"[[[3]]]" |> parse |> sum
"{\"a\":{\"b\":4},\"c\":-1}" |> parse |> sum
"{\"a\":[-1,1]}" |> parse |> sum
"[-1,{\"a\":1}]" |> parse |> sum
"[]" |> parse |> sum
"{}" |> parse |> sum


"{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}" |> parse |> sum2
"[1,{\"c\":\"red\",\"b\":2},3]" |> parse |> sum2
"{\"c\":\"red\",\"b\":2}" |> parse |> sum2

"day12.txt" |> part2