module Solver

open Board

// Takes a Value list in parameter and return a new list with all possible wich are duplicates of
// locked values removed
let removePossibleEntries (values:Value list) : Value list = 
    // printf "\nremovePossibleEntries entry %A" values

    // get locked values
    let locked = values |> List.filter (fun x -> match x with
                                                 | Locked _ -> true
                                                 | Possible _ -> false)
                        |> List.map (fun x -> match x with
                                              | Locked y -> y
                                              | Possible _ -> failwith "Should have been filtered out")                                             

    // remove locked values from all lists of possible values
    values |> List.map 
        (fun x -> match x with
                  | Locked y -> Locked y
                  | Possible items -> Possible (List.filter (fun z -> not (List.contains z locked)) items))

// go over all values and switch List of one possible value to a Locked value
let lockSingleValues (values:Value list) = 
    // printf "\nlockSingleValues entry %A" values
    values |> List.map (fun x -> match x with
                                 | Possible [a] -> Locked a
                                 | Possible (head :: tail) -> Possible (head :: tail)
                                 | Possible [] -> failwith "Error this Possible list is empty"
                                 | Locked v -> Locked v)

// when given a list of of lists each created by the application of the row function, convert it back to a board
let newBoardFromRows (items:Value list list) =
    Array2D.init 9 9 (fun i j -> items.[i].[8-j])

// when given a list of of lists each created by the application of the column function, convert it back to a board
let newBoardFromColumns (items:Value list list) =
    Array2D.init 9 9 (fun i j -> items.[j].[8-i])    

// when given a list of of lists each created by the application of the box function, convert it back to a board
let newBoardFromBox (items:Value list list) =
    let unboxed = Array2D.create 9 9 (Locked 0)
    
    let si = seq {
        for h in [0; 3; 6] do yield! seq {
            for i in 1..3 do yield! seq {
                for j in 1..3 do yield 0 + h
                for j in 1..3 do yield 1 + h
                for j in 1..3 do yield 2 + h
            }                
        }
    }   
    let sj = seq { 
        for i in 1..3 do yield! seq { 
            for j in 1..3 do yield! [0..2]; 
            for j in 1..3 do yield! [3..5]; 
            for j in 1..3 do yield! [6..8]; 
        }
    } 
    let sx = seq {
        for i in 0..8 do yield! seq { 
            for j in 0..8 do yield i, j
        }
    }
    
    let coordinates = List.map2 (fun i j -> i, j) (si |> Seq.toList) (sj |> Seq.toList)   
    List.iter2 (fun (x, y) (i, j) -> Array2D.set unboxed x y (items.[i].[8-j])) (sx |> Seq.toList) coordinates
    unboxed

// remove and lock values
// remove = remove elements in lists of possible values. We remove those values which already appear somewhere else in
//          the unit
// lock = when there is only one possible value we change it from a list of 1 possible to a locked value
let removeAndLock (board:Value[,]) = 
    let perform func dest =
        [0..8] |> List.map (fun i -> func i dest |> removePossibleEntries |> lockSingleValues)

    // perform row board |> perform column |> perform box
    perform row board |> newBoardFromRows |> perform column |> newBoardFromColumns |> perform box |> newBoardFromBox    

let solve (board:Value[,]) = 
    // call removeAndLock, if board has changed call it again in case we are able to remove more possible values
    // after the changes in the first removeAndLock call
    let rec exclusionStep previousStep = 
        let currentStep = removeAndLock previousStep
        if currentStep = previousStep then currentStep else exclusionStep currentStep

//     let rec depthFirstSearch previousStep =
//         // choose first unlocked square
        
//         // randomly pick first value
//         // verify solution

    let newBoard = exclusionStep board   
    // check if the board is solved otherwise try to solve the board by trying arbitrary values
    match boardSolved newBoard with
    // we can return this valid board
    | Solved -> newBoard    
    // we need to continue not all squares are filled
    | BoardUnlocked -> failwith "Board not yet solved"
    // we have come to an invalid solution after the exclusionStep, this shouldn't be happening
    | InvalidSolution -> failwith "InvalidSolution after performStep"