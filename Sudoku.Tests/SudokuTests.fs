module SudokuTests

open Board
open Solver
open TestFramework

let board = array2D [ 
                [| Locked 11; Locked 12; Locked 13; Locked 14; Locked 15; Locked 16; Locked 17; Locked 18; Locked 19 |];
                [| Locked 21; Locked 22; Locked 23; Locked 24; Locked 25; Locked 26; Locked 27; Locked 28; Locked 29 |];
                [| Locked 31; Locked 32; Locked 33; Locked 34; Locked 35; Locked 36; Locked 37; Locked 38; Locked 39 |];
                [| Locked 41; Locked 42; Locked 43; Locked 44; Locked 45; Locked 46; Locked 47; Locked 48; Locked 49 |];
                [| Locked 51; Locked 52; Locked 53; Locked 54; Locked 55; Locked 56; Locked 57; Locked 58; Locked 59 |];
                [| Locked 61; Locked 62; Locked 63; Locked 64; Locked 65; Locked 66; Locked 67; Locked 68; Locked 69 |];
                [| Locked 71; Locked 72; Locked 73; Locked 74; Locked 75; Locked 76; Locked 77; Locked 78; Locked 79 |];
                [| Locked 81; Locked 82; Locked 83; Locked 84; Locked 85; Locked 86; Locked 87; Locked 88; Locked 89 |];
                [| Locked 91; Locked 92; Locked 93; Locked 94; Locked 95; Locked 96; Locked 97; Locked 98; Locked 99 |]
            ]

let sudokuTests = 
    [
        "row returns the correct values for the first row",
        fun () -> 
            let firstRow = row 0 board
            assertAreEqual [Locked 19; Locked 18; Locked 17; Locked 16; Locked 15; Locked 14; Locked 13; Locked 12; Locked 11] 
                           firstRow;
            
        "row returns the correct values for the second row",
        fun () -> 
            let secondRow = row 1 board
            assertAreEqual [Locked 29; Locked 28; Locked 27; Locked 26; Locked 25; Locked 24; Locked 23; Locked 22; Locked 21] 
                           secondRow;                    

        "column returns the correct values for the third column",        
        fun () ->
            let thirdColumn = column 2 board
            assertAreEqual [Locked 93; Locked 83; Locked 73; Locked 63; Locked 53; Locked 43; Locked 33; Locked 23; Locked 13] 
                           thirdColumn;                    


        "column returns the correct values for the ninth column",        
        fun () ->
            let ninthColumn = column 8 board
            assertAreEqual [Locked 99; Locked 89; Locked 79; Locked 69; Locked 59; Locked 49; Locked 39; Locked 29; Locked 19] 
                           ninthColumn;         

        "box returns the correct values for the first box",
        fun () ->
            let firstBox = box 0 board                                  
            assertAreEqual [Locked 33; Locked 32; Locked 31; Locked 23; Locked 22; Locked 21; Locked 13; Locked 12; Locked 11] 
                           firstBox;         

        "box returns the correct values for the second box",
        fun () ->
            let secondBox = box 1 board                                  
            assertAreEqual [Locked 36; Locked 35; Locked 34; Locked 26; Locked 25; Locked 24; Locked 16; Locked 15; Locked 14] 
                           secondBox;         

        "box returns the correct values for the third box",
        fun () ->
            let thirdBox = box 2 board                                  
            assertAreEqual [Locked 39; Locked 38; Locked 37; Locked 29; Locked 28; Locked 27; Locked 19; Locked 18; Locked 17] 
                           thirdBox;                                

        "box returns the correct values for the fourth box",
        fun () ->
            let fourthBox = box 3 board                                  
            assertAreEqual [Locked 63; Locked 62; Locked 61; Locked 53; Locked 52; Locked 51; Locked 43; Locked 42; Locked 41] 
                           fourthBox;                                                           

        "box returns the correct values for the eight box",
        fun () ->
            let eightBox = box 7 board                                  
            assertAreEqual [Locked 96; Locked 95; Locked 94; Locked 86; Locked 85; Locked 84; Locked 76; Locked 75; Locked 74] 
                           eightBox;     

        "contains returns false if there are no duplicates",
        fun () -> 
            assertIsFalse (containsDuplicates [1; 2; 3; 4; 5])

        "contains returns true if there are duplicates",
        fun () ->
            assertIsTrue (containsDuplicates [1; 2; 3; 1; 4;])

        "boardSolved returns Solved for correctly solved board #1",
        fun () -> 
            let board =  
                array2D [ 
                    [| Locked 4; Locked 3; Locked 5; Locked 2; Locked 6; Locked 9; Locked 7; Locked 8; Locked 1 |];
                    [| Locked 6; Locked 8; Locked 2; Locked 5; Locked 7; Locked 1; Locked 4; Locked 9; Locked 3 |];
                    [| Locked 1; Locked 9; Locked 7; Locked 8; Locked 3; Locked 4; Locked 5; Locked 6; Locked 2 |];
                    [| Locked 8; Locked 2; Locked 6; Locked 1; Locked 9; Locked 5; Locked 3; Locked 4; Locked 7 |];
                    [| Locked 3; Locked 7; Locked 4; Locked 6; Locked 8; Locked 2; Locked 9; Locked 1; Locked 5 |];
                    [| Locked 9; Locked 5; Locked 1; Locked 7; Locked 4; Locked 3; Locked 6; Locked 2; Locked 8 |];
                    [| Locked 5; Locked 1; Locked 9; Locked 3; Locked 2; Locked 6; Locked 8; Locked 7; Locked 4 |];
                    [| Locked 2; Locked 4; Locked 8; Locked 9; Locked 5; Locked 7; Locked 1; Locked 3; Locked 6 |];
                    [| Locked 7; Locked 6; Locked 3; Locked 4; Locked 1; Locked 8; Locked 2; Locked 5; Locked 9 |]
                ]                                                     
            assertAreEqual Solved (boardSolved board);            

        "boardSolved returns Solved for correctly solved board #2",
        fun () -> 
            let board =  
                array2D [ 
                    [| Locked 1; Locked 5; Locked 2; Locked 4; Locked 8; Locked 9; Locked 3; Locked 7; Locked 6 |];
                    [| Locked 7; Locked 3; Locked 9; Locked 2; Locked 5; Locked 6; Locked 8; Locked 4; Locked 1 |];
                    [| Locked 4; Locked 6; Locked 8; Locked 3; Locked 7; Locked 1; Locked 2; Locked 9; Locked 5 |];
                    [| Locked 3; Locked 8; Locked 7; Locked 1; Locked 2; Locked 4; Locked 6; Locked 5; Locked 9 |];
                    [| Locked 5; Locked 9; Locked 1; Locked 7; Locked 6; Locked 3; Locked 4; Locked 2; Locked 8 |];
                    [| Locked 2; Locked 4; Locked 6; Locked 8; Locked 9; Locked 5; Locked 7; Locked 1; Locked 3 |];
                    [| Locked 9; Locked 1; Locked 4; Locked 6; Locked 3; Locked 7; Locked 5; Locked 8; Locked 2 |];
                    [| Locked 6; Locked 2; Locked 5; Locked 9; Locked 4; Locked 8; Locked 1; Locked 3; Locked 7 |];
                    [| Locked 8; Locked 7; Locked 3; Locked 5; Locked 1; Locked 2; Locked 9; Locked 6; Locked 4 |]
                ]                                                     
            assertAreEqual Solved (boardSolved board);

        "boardSolved returns Solved for correctly solved board #3",
        fun () -> 
            let board =  
                array2D [ 
                    [| Locked 1; Locked 2; Locked 3; Locked 6; Locked 7; Locked 8; Locked 9; Locked 4; Locked 5 |];
                    [| Locked 5; Locked 8; Locked 4; Locked 2; Locked 3; Locked 9; Locked 7; Locked 6; Locked 1 |];
                    [| Locked 9; Locked 6; Locked 7; Locked 1; Locked 4; Locked 5; Locked 3; Locked 2; Locked 8 |];
                    [| Locked 3; Locked 7; Locked 2; Locked 4; Locked 6; Locked 1; Locked 5; Locked 8; Locked 9 |];
                    [| Locked 6; Locked 9; Locked 1; Locked 5; Locked 8; Locked 3; Locked 2; Locked 7; Locked 4 |];
                    [| Locked 4; Locked 5; Locked 8; Locked 7; Locked 9; Locked 2; Locked 6; Locked 1; Locked 3 |];
                    [| Locked 8; Locked 3; Locked 6; Locked 9; Locked 2; Locked 4; Locked 1; Locked 5; Locked 7 |];
                    [| Locked 2; Locked 1; Locked 9; Locked 8; Locked 5; Locked 7; Locked 4; Locked 3; Locked 6 |];
                    [| Locked 7; Locked 4; Locked 5; Locked 3; Locked 1; Locked 6; Locked 8; Locked 9; Locked 2 |]
                ]                                                     
            assertAreEqual Solved (boardSolved board);

        "boardSolved returns InvalidSolution when duplicates in one row",
        fun () -> 
            let board =  
                array2D [ 
                    [| Locked 1; Locked 5; Locked 2; Locked 4; Locked 1; Locked 9; Locked 3; Locked 7; Locked 6 |];
                    [| Locked 7; Locked 3; Locked 9; Locked 2; Locked 5; Locked 6; Locked 8; Locked 4; Locked 1 |];
                    [| Locked 4; Locked 6; Locked 8; Locked 3; Locked 7; Locked 1; Locked 2; Locked 9; Locked 5 |];
                    [| Locked 3; Locked 8; Locked 7; Locked 1; Locked 2; Locked 4; Locked 6; Locked 5; Locked 9 |];
                    [| Locked 5; Locked 9; Locked 1; Locked 7; Locked 6; Locked 3; Locked 4; Locked 2; Locked 8 |];
                    [| Locked 2; Locked 4; Locked 6; Locked 8; Locked 9; Locked 5; Locked 7; Locked 1; Locked 3 |];
                    [| Locked 9; Locked 1; Locked 4; Locked 6; Locked 3; Locked 7; Locked 5; Locked 8; Locked 2 |];
                    [| Locked 6; Locked 2; Locked 5; Locked 9; Locked 4; Locked 8; Locked 1; Locked 3; Locked 7 |];
                    [| Locked 8; Locked 7; Locked 3; Locked 5; Locked 1; Locked 2; Locked 9; Locked 6; Locked 4 |]
                ]                                                     
            assertAreEqual InvalidSolution (boardSolved board);

        "boardSolved returns InvalidSolution when duplicates in one row #2",
        fun () -> 
            let board =  
                array2D [ 
                    [| Locked 1; Locked 2; Locked 3; Locked 6; Locked 7; Locked 8; Locked 9; Locked 4; Locked 5 |];
                    [| Locked 5; Locked 8; Locked 4; Locked 2; Locked 3; Locked 9; Locked 7; Locked 6; Locked 1 |];
                    [| Locked 9; Locked 6; Locked 7; Locked 1; Locked 4; Locked 5; Locked 3; Locked 2; Locked 8 |];
                    [| Locked 3; Locked 7; Locked 2; Locked 4; Locked 6; Locked 1; Locked 5; Locked 8; Locked 9 |];
                    [| Locked 6; Locked 9; Locked 1; Locked 5; Locked 8; Locked 3; Locked 2; Locked 7; Locked 4 |];
                    [| Locked 4; Locked 5; Locked 8; Locked 7; Locked 9; Locked 2; Locked 6; Locked 1; Locked 3 |];
                    [| Locked 8; Locked 3; Locked 6; Locked 8; Locked 2; Locked 4; Locked 1; Locked 5; Locked 7 |];
                    [| Locked 2; Locked 1; Locked 9; Locked 8; Locked 5; Locked 7; Locked 4; Locked 3; Locked 6 |];
                    [| Locked 7; Locked 4; Locked 5; Locked 3; Locked 1; Locked 6; Locked 8; Locked 9; Locked 2 |]
                ]                                                     
            assertAreEqual InvalidSolution (boardSolved board);    

        "boardSolved returns InvalidSolution when duplicates in one column",
        fun () -> 
            let board =  
                array2D [ 
                    [| Locked 1; Locked 5; Locked 2; Locked 4; Locked 8; Locked 9; Locked 3; Locked 7; Locked 6 |];
                    [| Locked 7; Locked 3; Locked 9; Locked 2; Locked 5; Locked 6; Locked 8; Locked 4; Locked 1 |];
                    [| Locked 4; Locked 6; Locked 8; Locked 3; Locked 7; Locked 1; Locked 2; Locked 9; Locked 5 |];
                    [| Locked 3; Locked 8; Locked 7; Locked 1; Locked 2; Locked 4; Locked 6; Locked 5; Locked 9 |];
                    [| Locked 5; Locked 9; Locked 1; Locked 7; Locked 6; Locked 3; Locked 4; Locked 2; Locked 8 |];
                    [| Locked 2; Locked 5; Locked 6; Locked 8; Locked 9; Locked 5; Locked 7; Locked 1; Locked 3 |];
                    [| Locked 9; Locked 1; Locked 4; Locked 6; Locked 3; Locked 7; Locked 5; Locked 8; Locked 2 |];
                    [| Locked 6; Locked 2; Locked 5; Locked 9; Locked 4; Locked 8; Locked 1; Locked 3; Locked 7 |];
                    [| Locked 8; Locked 7; Locked 3; Locked 5; Locked 1; Locked 2; Locked 9; Locked 6; Locked 4 |]
                ]                                                     
            assertAreEqual InvalidSolution (boardSolved board);

        "boardSolved returns InvalidSolution for duplicates in one column #2",
        fun () -> 
            let board =  
                array2D [ 
                    [| Locked 4; Locked 3; Locked 5; Locked 2; Locked 6; Locked 9; Locked 7; Locked 8; Locked 1 |];
                    [| Locked 6; Locked 8; Locked 2; Locked 5; Locked 7; Locked 1; Locked 4; Locked 9; Locked 3 |];
                    [| Locked 1; Locked 9; Locked 7; Locked 8; Locked 3; Locked 4; Locked 5; Locked 6; Locked 2 |];
                    [| Locked 8; Locked 2; Locked 6; Locked 1; Locked 9; Locked 5; Locked 3; Locked 4; Locked 7 |];
                    [| Locked 3; Locked 7; Locked 4; Locked 6; Locked 8; Locked 2; Locked 9; Locked 1; Locked 9 |];
                    [| Locked 9; Locked 5; Locked 1; Locked 7; Locked 4; Locked 3; Locked 6; Locked 2; Locked 8 |];
                    [| Locked 5; Locked 1; Locked 9; Locked 3; Locked 2; Locked 6; Locked 8; Locked 7; Locked 4 |];
                    [| Locked 2; Locked 4; Locked 8; Locked 9; Locked 5; Locked 7; Locked 1; Locked 3; Locked 6 |];
                    [| Locked 7; Locked 6; Locked 3; Locked 4; Locked 1; Locked 8; Locked 2; Locked 5; Locked 9 |]
                ]                                                     
            assertAreEqual InvalidSolution (boardSolved board);          

        "boardSolved returns InvalidSolution when duplicates in one box",
        fun () -> 
            let board =  
                array2D [ 
                    [| Locked 1; Locked 5; Locked 2; Locked 4; Locked 8; Locked 9; Locked 3; Locked 7; Locked 6 |];
                    [| Locked 7; Locked 3; Locked 9; Locked 2; Locked 5; Locked 6; Locked 8; Locked 4; Locked 1 |];
                    [| Locked 4; Locked 6; Locked 8; Locked 3; Locked 7; Locked 1; Locked 2; Locked 9; Locked 5 |];
                    [| Locked 3; Locked 8; Locked 7; Locked 1; Locked 2; Locked 4; Locked 6; Locked 5; Locked 9 |];
                    [| Locked 5; Locked 9; Locked 1; Locked 7; Locked 6; Locked 3; Locked 4; Locked 2; Locked 8 |];
                    [| Locked 2; Locked 4; Locked 6; Locked 8; Locked 9; Locked 5; Locked 7; Locked 1; Locked 3 |];
                    [| Locked 9; Locked 1; Locked 4; Locked 6; Locked 3; Locked 7; Locked 5; Locked 4; Locked 2 |];
                    [| Locked 6; Locked 2; Locked 5; Locked 9; Locked 4; Locked 8; Locked 1; Locked 3; Locked 7 |];
                    [| Locked 8; Locked 7; Locked 3; Locked 5; Locked 1; Locked 2; Locked 9; Locked 6; Locked 4 |]
                ]                                                     
            assertAreEqual InvalidSolution (boardSolved board);

        "boardSolved returns InvalidSolution for multipe duplicates #1",
        fun () -> 
            let board =  
                array2D [ 
                    [| Locked 1; Locked 5; Locked 2; Locked 4; Locked 8; Locked 9; Locked 3; Locked 7; Locked 6 |];
                    [| Locked 7; Locked 3; Locked 9; Locked 2; Locked 5; Locked 6; Locked 8; Locked 4; Locked 1 |];
                    [| Locked 4; Locked 6; Locked 8; Locked 3; Locked 7; Locked 2; Locked 2; Locked 9; Locked 5 |];
                    [| Locked 3; Locked 8; Locked 7; Locked 3; Locked 2; Locked 4; Locked 6; Locked 5; Locked 9 |];
                    [| Locked 5; Locked 7; Locked 1; Locked 7; Locked 6; Locked 3; Locked 4; Locked 2; Locked 8 |];
                    [| Locked 2; Locked 4; Locked 6; Locked 8; Locked 9; Locked 5; Locked 7; Locked 1; Locked 3 |];
                    [| Locked 8; Locked 1; Locked 4; Locked 6; Locked 3; Locked 7; Locked 5; Locked 4; Locked 2 |];
                    [| Locked 6; Locked 2; Locked 5; Locked 9; Locked 4; Locked 8; Locked 1; Locked 3; Locked 7 |];
                    [| Locked 8; Locked 7; Locked 3; Locked 5; Locked 1; Locked 1; Locked 8; Locked 6; Locked 4 |]
                ]                                                     
            assertAreEqual InvalidSolution (boardSolved board);

        "removePossibleEntries will remove entries from the Possible lists which are already locked #1",
        fun () ->             
            let possibles = [Locked 1; Possible [1; 2 ;3 ;4 ;5 ;6 ]; Locked 3; Possible [2; 3; 4; 6; 8; 9]]
            let result = removePossibleEntries possibles
            assertAreEqual [Locked 1; Possible [2; 4; 5; 6]; Locked 3; Possible [2; 4; 6; 8; 9]] result;

        "removePossibleEntries will remove entries from the Possible lists which are already locked #2",
        fun () ->             
            let possibles = [Locked 5; Possible [1; 2 ;3 ;4 ;5 ;6 ]; Locked 3; Possible [2; 3; 4; 6; 8; 9]; 
                             Possible [4; 5; 7; 9]; Locked 6]
            let result = removePossibleEntries possibles
            assertAreEqual [Locked 5; Possible [1; 2 ;4]; Locked 3; Possible [2; 4; 8; 9]; Possible [4; 7; 9]; Locked 6] 
                           result;

        "removePossibleEntries will not remove entries when none are to be removed",
        fun () ->             
            let possibles = [Locked 5; Possible [1; 2 ;4 ]; Locked 3; Possible [2; 4; 8; 9]; 
                             Possible [4; 7; 9]; Locked 6]
            let result = removePossibleEntries possibles
            assertAreEqual [Locked 5; Possible [1; 2 ;4]; Locked 3; Possible [2; 4; 8; 9]; Possible [4; 7; 9]; Locked 6] 
                           result;

        "lockSingleValues will lock out single values",
        fun () ->
            let possibles = [Locked 5; Possible [1; 2 ;3 ;4 ;5 ;6 ]; Locked 3; Possible [2; 3; 4; 6; 8; 9]; 
                             Possible [4]; Possible [6]]
            let result = lockSingleValues possibles           
            assertAreEqual  [Locked 5; Possible [1; 2 ;3 ;4 ;5 ;6 ]; Locked 3; Possible [2; 3; 4; 6; 8; 9]; 
                             Locked 4; Locked 6]               
                            result;

        // "test recreate array from list of lists",
        // fun () ->
        //     let newRows = [0..8] |> List.map (fun i -> row i board |> removePossibleEntries |> lockSingleValues)
        //     let x = Array2D.init 9 9 (fun i j -> newRows.[8-i].[8-j] )
        //     board

        "removeAndLock will return the board in the same order",    // my original implementation reversed it causing
        fun () ->                                                   // all sorts of nasties
            let result = removeAndLock board
            assertAreEqual board result
        // "easy board that can be solved without depth search",
        // fun () ->
        //     let allNumbers = [1; 2; 3; 4; 5; 6; 7; 8; 9]
        //     let board =  
        //         array2D [ 
        //             [| Possible allNumbers ; Possible allNumbers; Locked 3; Possible allNumbers; Locked 2; Possible allNumbers; Locked 6; Possible allNumbers; Possible allNumbers |];
        //             [| Locked 9; Possible allNumbers; Possible allNumbers; Locked 3; Possible allNumbers; Locked 5; Possible allNumbers; Possible allNumbers; Locked 1 |];
        //             [| Possible allNumbers; Possible allNumbers; Locked 1; Locked 8; Possible allNumbers; Locked 6; Locked 4; Possible allNumbers; Possible allNumbers |];
        //             [| Possible allNumbers; Possible allNumbers; Locked 8; Locked 1; Possible allNumbers; Locked 2; Locked 9; Possible allNumbers; Possible allNumbers |];
        //             [| Locked 7; Possible allNumbers; Possible allNumbers; Possible allNumbers; Possible allNumbers; Possible allNumbers; Possible allNumbers; Possible allNumbers; Locked 8 |];
        //             [| Possible allNumbers; Possible allNumbers; Locked 6; Locked 7; Possible allNumbers; Locked 8; Locked 2; Possible allNumbers; Possible allNumbers |];
        //             [| Possible allNumbers; Possible allNumbers; Locked 2; Locked 6; Possible allNumbers; Locked 9; Locked 5; Possible allNumbers; Possible allNumbers |];
        //             [| Locked 8; Possible allNumbers; Possible allNumbers; Locked 2; Possible allNumbers; Locked 3; Possible allNumbers; Possible allNumbers; Locked 9 |];
        //             [| Possible allNumbers; Possible allNumbers; Locked 5; Possible allNumbers; Locked 1; Possible allNumbers; Locked 3; Possible allNumbers; Possible allNumbers |]
        //         ] 
        //     let solved = solve board
        //     let solution = 
        //         array2D [ 
        //                 [| Locked 4 ; Locked 8; Locked 3; Locked 9; Locked 2; Locked 1; Locked 6; Locked 6; Locked 7 |];
        //                 [| Locked 9; Locked 6; Locked 7; Locked 3; Locked 4; Locked 5; Locked 8; Locked 2; Locked 1 |];
        //                 [| Locked 2; Locked 5; Locked 1; Locked 8; Locked 7; Locked 6; Locked 4; Locked 9; Locked 3 |];
        //                 [| Locked 5; Locked 4; Locked 8; Locked 1; Locked 3; Locked 2; Locked 9; Locked 7; Locked 6 |];
        //                 [| Locked 7; Locked 2; Locked 9; Locked 5; Locked 6; Locked 4; Locked 1; Locked 3; Locked 8 |];
        //                 [| Locked 1; Locked 3; Locked 6; Locked 7; Locked 9; Locked 8; Locked 2; Locked 4; Locked 5 |];
        //                 [| Locked 3; Locked 7; Locked 2; Locked 6; Locked 8; Locked 9; Locked 5; Locked 1; Locked 4 |];
        //                 [| Locked 8; Locked 1; Locked 4; Locked 2; Locked 5; Locked 3; Locked 7; Locked 6; Locked 9 |];
        //                 [| Locked 6; Locked 9; Locked 5; Locked 4; Locked 1; Locked 7; Locked 3; Locked 8; Locked 2 |]
        //             ] 
        //     assertAreEqual solved solution;            
    ]