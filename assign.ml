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
let reverse_string str =
  let len = String.length str in
  let res = ref "" in
  let curr = ref "" in
  for i = 0 to len - 1 do
    if str.[i] = ' ' then (
      res := !curr ^ " " ^ !res; 
      curr := "" 
    )
    else (
      curr := !curr ^ String.make 1 str.[i] 
    )
  done;
  !res ^ !curr 


(* This function reverses the input string list
let reverse_string_list lines = *)


let () =
  print_endline "Choos input method";
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
    List.iter print_endline lines
    print_endline "The output text is:";
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
    List.iter print_endline lines
    print_endline "The output text is:";
  )
  else (
    print_endline "Invalid input"
  )