open Printf

(*This function takes the filename as input and returns the contents of the file*)
let read_file_contents filename =
  let channel = open_in filename in
  let file_contents =
    try
      let rec read_lines acc =
        let line = input_line channel in
        read_lines (line :: acc)
      in
      let lines = read_lines [] in
      String.concat "\n" (List.rev lines)
    with End_of_file ->
      close_in channel;
      ""
  in
  file_contents


let () =
  print_endline "Choos input method";
  print_endline "1. For input from file";
  print_endline "2. For input from command line";
  print_endline "->";
  let choice : string = read_line () in
  if choice = "1" then (
    print_endline "Enter the filename:";
    let filename : string = read_line () in
    let text : string = read_file_contents filename in
    printf "This is your input:\n%s" text
  )
  else if choice = "2" then (
    print_endline "Enter the text here:";
    let text : string = read_line () in
    printf "This is your input:\n%s" text
  )
  else (
    print_endline "Invalid input"
  )