let solution xs =
    let tail = List.tail xs
    let init = List.removeAt (List.length xs - 1) xs
    List.zip init tail
    |> List.map (fun (x,y) -> (y - x))
    |> List.filter (fun x -> x > 0)
    |> List.length

let solution' xs =
    let tail = List.tail xs
               |> List.tail
    let mid = List.removeAt (List.length xs - 1) xs
              |> List.tail
    let head = List.removeAt (List.length xs - 1) xs
               |> List.removeAt (List.length xs - 2)
    List.zip3 tail mid head
    |> List.map (fun (x,y,z) -> (x + y + z))
    |> solution

let readIntList filename =
    System.IO.File.ReadLines(filename)
    |> Seq.map (fun x -> int x)
    |> Seq.toList


let input = readIntList "input.txt"
let result = solution input
let result' = solution' input
printfn "result: %d" result
printfn "result': %d" result'
