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


(* This function is used to reverse a string *)
let reverse_word word =
  let char_list = List.init (String.length word) (fun i -> String.make 1 word.[i]) in
  let reversed_char_list = List.rev char_list in
  String.concat "" reversed_char_list


(* Split the input string into words *)
let reverse_string str =
  let words = String.split_on_char ' ' str in
  let reversed_words = List.map reverse_word words in
  String.concat " " reversed_words


(* This function reverses the input string list *)
let reverse_string_list lines =
  List.map reverse_string lines


let () =
  print_endline "Choose input method:";
  print_endline "1. For input from file";
  print_endline "2. For input from command line";
  print_endline "->";
  let choice : string = read_line () in
  if choice = "1" then (
    print_endline "Enter the filename:";
    print_string "> ";
    let filename : string = read_line () in
    let lines = read_from_file filename in
    print_endline "The input text is:";
    List.iter print_endline lines;
    print_endline "The output text is:";
    let reversed_lines = reverse_string_list lines in
    List.iter print_endline reversed_lines
  )
  else if choice = "2" then (
    print_endline "Enter your text (press Enter twice to finish):";
    let rec read_lines acc =
      let line = read_line () in
      if line = "" then List.rev acc
      else read_lines (line :: acc)
    in
    let lines = read_lines [] in
    print_endline "The input text is:";
    List.iter print_endline lines;
    print_endline "The output text is:";
    let reversed_lines = reverse_string_list lines in
    List.iter print_endline reversed_lines
  )
  else (
    print_endline "Invalid input"
  )
