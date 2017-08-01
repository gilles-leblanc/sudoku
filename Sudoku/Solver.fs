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
