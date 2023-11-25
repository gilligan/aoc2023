open Alcotest

let suites = List.flatten [ Day1test.suite ; Utiltest.suite ]
let () = run "AllTests" suites
