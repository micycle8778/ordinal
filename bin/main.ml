open Ordinal

let get_char () =
    let termio = Unix.tcgetattr Unix.stdin in
    termio.c_echo <- false;
    let () =
        Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
            { termio with Unix.c_icanon = false } in
    let res = input_char stdin in
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
    termio.c_echo <- true; Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
    res

let () =
  let has_printed = ref false in
  let put_string s = has_printed := true; print_string s; flush stdout in
  let rec main parser interpreter = 
    has_printed := false;
    if parser.comment_count = 0
    then print_string ">>> "
    else print_string "(comment)>> ";
    let line = read_line () in
    (* it's our job as the top level to keep the memory counts in sync between the parser and the interpreter *)
    (match parse_words { parser with memory_count = Array.length interpreter.memory } (split_string line) with
    | Ok (parser, words) -> 
      (* it's our job as the top level to keep the memory counts in sync between the parser and the interpreter *)
      let interpreter = { interpreter with memory = adjust_arr interpreter.memory parser.memory_count (Int 0) } in
        (match interpret_words interpreter words with
        | Ok interpreter -> if !has_printed then print_newline (); main parser interpreter (* If everything is OK, we'll end up here *)
        | Error e -> print_endline (string_of_interpreter_error e))
    | Error e -> print_endline (string_of_parser_error e));
    main parser interpreter (* If we error, we will end up here *)
  in
  let parser = { comment_count = 0; state = []; words = Hashtbl.create 5; memory_count = 0 } in
  let interpreter = { stack = []; i = Int 1; memory = [||]; get_char; put_string } in
  main parser interpreter