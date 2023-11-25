open Aoc
open Alcotest

let fact_test input expected () =
  let result = Day1.foo input in
  check int (Printf.sprintf "%d! == %d" input expected) result expected

let suite =
  [
    ("1! == 1", [ test_case "1" `Quick (fact_test 1 1) ]);
    ("2! == 2", [ test_case "2" `Quick (fact_test 2 2) ]);
  ]
