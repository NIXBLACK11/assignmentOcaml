open Printf

(* This function takes the filename as input and returns the contents of the file *)
let read_from_file filename =
  try
    let ic = open_in filename in
    let rec read_lines acc =
      try
        let line = input_line ic in
        read_lines (line :: acc)
      with End_of_file ->
        close_in ic;
        List.rev acc
    in
    read_lines []
  with Sys_error msg ->
    printf "Error: %s\n" msg;
    []

let () =
  print_endline "Choose input method:";
  print_endline "1. From file";
  print_endline "2. From command line";
  print_string "> ";
  let choice = read_line () in
  if choice = "1" then (
    print_endline "Enter the filename:";
    print_string "> ";
    let filename = read_line () in
    let lines = read_from_file filename in
    List.iter print_endline lines
  )
  else if choice = "2" then (
    print_endline "Enter your text (press Enter twice to finish):";
    let rec read_lines acc =
      let line = read_line () in
      if line = "" then List.rev acc
      else read_lines (line :: acc)
    in
    let lines = read_lines [] in
    List.iter print_endline lines
  )
  else (
    print_endline "Invalid choice"
  )