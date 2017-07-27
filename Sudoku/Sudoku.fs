module Sudoku

type Value =
    | Locked of int
    | Possible of int[]   

let private getLinearElements eFunc start end' (board:Value[,]) : Value list =
    [start..end'] |> List.fold (eFunc) [] 

let row rowNum (board:Value[,]) : Value list =
    getLinearElements (fun acc e -> board.[rowNum, e] :: acc) 0 8 board

let column colNum (board:Value[,]) : Value list =
    getLinearElements (fun acc e -> board.[e, colNum] :: acc) 0 8 board

let box boxNum (board:Value[,]) : Value list =
    let startX, endX, startY, endY = match boxNum with
                                     | 0 -> 0, 2, 0, 2
                                     | 1 -> 3, 5, 0, 2
                                     | 2 -> 6, 8, 0, 2                                  
                                     | 3 -> 0, 2, 3, 5
                                     | 4 -> 3, 5, 3, 5
                                     | 5 -> 6, 8, 3, 5                                  
                                     | 6 -> 0, 2, 6, 8
                                     | 7 -> 3, 5, 6, 8
                                     | 8 -> 6, 8, 6, 8
                                     | _ -> failwith "Invalid box values. range: 0-8"

    [startY..endY] 
    |> List.fold (fun acc x -> (getLinearElements (fun acc e -> board.[x, e] :: acc) startX endX board) @ acc) []    

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    0 // return an integer exit code
    