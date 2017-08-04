module Solver

open Board

// Takes a Value list in parameter and return a new list with all possible wich are duplicates of
// locked values removed
let removePossibleEntries (values:Value list) : Value list = 
    // get locked values
    let locked = values |> List.filter (fun x -> match x with
                                                 | Locked _ -> true
                                                 | Possible _ -> false)
                        |> List.map (fun x -> match x with
                                              | Locked y -> y
                                              | Possible _ -> failwith "Should have been filtered out")                                             

    // remove locked values from all list of possible values
    values |> List.map 
        (fun x -> match x with
                  | Locked y -> Locked y
                  | Possible items -> Possible (List.filter (fun z -> not (List.contains z locked)) items))

// go over all values and switch List of one possible value to a Locked value
let lockSingleValues (values:Value list) = 
    values |> List.map (fun x -> match x with
                                 | Possible [a] -> Locked a
                                 | Possible (head :: tail) -> Possible (head :: tail)
                                 | Possible [] -> failwith "Error this Possible list is empty"
                                 | Locked v -> Locked v)

let removeAndLock (board:Value[,]) = 
    let newRows = [0..8] |> List.map (fun i -> row i board |> removePossibleEntries |> lockSingleValues)
    let newBoard = Array2D.init 9 9 (fun i j -> newRows.[i].[8-j] )
    
    let newCols = [0..8] |> List.map (fun i -> column i board |> removePossibleEntries |> lockSingleValues)
    let newBoard' = Array2D.init 9 9 (fun i j -> newCols.[i].[8-j] )

    let newBox = [0..8] |> List.map (fun i -> box i board |> removePossibleEntries |> lockSingleValues)
    Array2D.init 9 9 (fun i j -> newBox.[i].[8-j] )

    // let newBoard' = array2D [ [0..8] |> List.map (fun i -> column i newBoard |> removePossibleEntries |> lockSingleValues) ]
    // array2D [ [0..8] |> List.map (fun i -> box i newBoard' |> removePossibleEntries |> lockSingleValues) ]       
    