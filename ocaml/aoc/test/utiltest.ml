open Aoc
open Alcotest

let read_numbers () =
  let result = Util.read_file_lines_as_ints "../data/numbers.txt"
  in check (list int) "" result [1;2;3]

let read_strings () =
  let id x = x in
  let result = Util.read_file_lines_parsed_with "../data/numbers.txt" id
  in check (list string) "" result ["1";"2";"3"]

let suite =
  [
      ("read_numbers: Reads [1;2;3] from numbers.txt", [ test_case "123" `Quick (read_numbers);  ]);
      ("Reads strings from numbers.txt", [ test_case "123" `Quick (read_strings) ]);
  ]
