module Sudoku.Tests

open TestFramework
open SudokuTests

[<EntryPoint>]
let main argv =
    consoleTestRunner sudokuTests
    0 // return an integer exit code
