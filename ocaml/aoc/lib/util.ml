module Util = struct
  open Core

  let read_file_lines_parsed_with filepath f =
    In_channel.read_lines filepath |> List.map ~f

  let read_file_lines_as_ints filepath =
    read_file_lines_parsed_with filepath int_of_string
end
