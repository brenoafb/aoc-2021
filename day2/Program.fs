type Direction =
    | Forward
    | Down
    | Up

let words (s: string) : string list =
    s.Split [| ' ' |] |> Seq.toList

let parseDirection (s: string) : Direction =
    match s with
        | "forward" -> Forward
        | "down" -> Down
        | "up" -> Up

let parseLine line : (Direction * int) =
    let tokens = words line
    match tokens with
        | [dir; n] -> (parseDirection dir, int n)

let parseInput filename : (Direction * int) list =
    System.IO.File.ReadLines(filename)
    |> Seq.map parseLine
    |> Seq.toList


let solution input =
    let f acc v =
        match acc with
            | (x, y) ->
                match v with
                    | (Forward, n) -> (x + n, y)
                    | (Down, n)    -> (x, y + n)
                    | (Up, n)      -> (x, y - n)
    input
    |> List.fold f (0, 0)
    |> (fun tup -> fst tup * snd tup)

// For more information see https://aka.ms/fsharp-console-apps


let input = parseInput "input.txt"
let result = solution input
printfn "result: %d" result
