type value =
  | Int of int
  | Float of float

type interpreter = {
  stack: value list;
  i: value; (* value used in do loops *)
  memory: (int, value) Hashtbl.t;
  has_printed: bool
}

type interpreter_error =
  | StackUnderflow
  | TypeError of value
  | Break of interpreter
  | InaccessibleDereference
  | Unreachable (* If this error is instanced, that means there is a bug *)

type word = (* All of the language constructs *)
  | Value of value
  | Builtin of (interpreter -> (interpreter, interpreter_error) result)
  | Subroutine of word list
  | DoLoop of word list
  | BeginUntilLoop of word list
  | IfThen of word list
  | IfElseThen of word list * word list

type parser_state = 
  | WordDefinition of string option * word list
  | DoLoop of word list
  | BeginUntilLoop of word list
  | IfThen of word list
  | IfElseThen of word list * word list
  | String of string list

type parser_error =
  | MisplacedCommentClose
  | MisplacedWordDefinition
  | MisplacedWordDefinitionClose
  | MisplacedStringClose
  | MisplacedLoop
  | MisplacedUntil
  | MisplacedThen
  | MisplacedElse
  | UnnamedWordDefinition
  | UnknownWord of string
  | Unreachable

type parser = {
  comment_count: int;
  state: parser_state list;
  words: (string, word list) Hashtbl.t
}

let getchar () =
    let termio = Unix.tcgetattr Unix.stdin in
    termio.c_echo <- false;
    let () =
        Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
            { termio with Unix.c_icanon = false } in
    let res = input_char stdin in
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
    termio.c_echo <- true; Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
    res

let string_of_string_list ss = 
  let rec aux acc = function
  | [] -> acc
  | s :: ss -> aux (acc ^ s ^ " ") ss
  in 
  let s = aux "" ss in
  String.sub s 0 ((String.length s) - 1) (* Remove the space at the end *)


let string_of_parser_error = function
| MisplacedCommentClose -> "MisplacedCommentClose"
| MisplacedWordDefinition -> "MisplacedWordDefinition"
| MisplacedWordDefinitionClose -> "MisplacedWordDefinitionClose"
| MisplacedStringClose -> "MisplacedStringClose"
| MisplacedLoop -> "MisplacedLoop"
| MisplacedUntil -> "MisplacedUntil"
| MisplacedThen -> "MisplacedThen"
| MisplacedElse -> "MisplacedElse"
| UnnamedWordDefinition -> "UnnamedWordDefinition"
| UnknownWord _ -> "UnknownWord"
| Unreachable -> "Unreachable"

let string_of_interpreter_error = function
| StackUnderflow -> "StackUnderflow"
| TypeError _ -> "TypeError"
| Break _ -> "MisplacedBreak"
| InaccessibleDereference -> "InaccessibleDereference"
| Unreachable -> "Unreachable"

(* helper value functions *)
let get_float = function
| Int i -> float_of_int i
| Float f -> f

let value_of_bool = function
| true -> Int (-1)
| false -> Int 0

let bool_of_value = function
| Int i -> i != 0
| Float f -> f != 0.0

let equal v1 v2 =
  match v1 with
  | Int i1 -> (match v2 with
    | Int i2 -> i1 = i2
    | Float f2 -> (float_of_int i1) = f2)
  | Float f1 -> f1 = (get_float v2)

let not_equal v1 v2 = not (equal v1 v2)

let less_than v1 v2 =
  match v1 with
  | Int i1 -> (match v2 with
    | Int i2 -> i1 < i2
    | Float f2 -> (float_of_int i1) < f2)
  | Float f1 -> f1 < (get_float v2)

let less_than_equal v1 v2 = (less_than v1 v2) || (equal v1 v2)

let greater_than v1 v2 = not (less_than_equal v1 v2)

let greater_than_equal v1 v2 = not (less_than v1 v2)

(* stack management *)
let push interpreter v = { stack = v :: interpreter.stack; i = interpreter.i; memory = interpreter.memory; has_printed = interpreter.has_printed }

let rec pushs interpreter = function
| [] -> interpreter
| v :: vs -> pushs (push interpreter v) vs

let pop interpreter =
  match interpreter.stack with
  | [] -> Error StackUnderflow
  | v :: stack -> Ok (v, { stack = stack; i = interpreter.i; memory = interpreter.memory; has_printed = interpreter.has_printed })

let pop_n interpreter n =
  let rec aux acc interpreter = function
  | 0 -> Ok (acc, interpreter)
  | n -> match pop interpreter with
    | Ok (v, i) -> aux (v :: acc) i (n - 1)
    | Error e -> Error e
  in
  aux [] interpreter n

(* interpreter.has_printed *)
let set_has_printed interpreter value =
  { stack = interpreter.stack; i = interpreter.i; memory = interpreter.memory; has_printed = value }

(* Split a string by whitespace *)
let split_string s = 
  let rec aux cs ss = function
  | [] -> String.of_seq (List.to_seq (List.rev cs)) :: ss
  | c :: t -> match c with
    | ' ' | '\n' | '\t' | '\r' -> aux [] (String.of_seq (List.to_seq (List.rev cs)) :: ss) t
    | c -> aux (c :: cs) ss t
  in
  List.rev (aux [] [] (List.init (String.length s) (String.get s)))

let builtin_of_string string =
  let math_fn intfn floatfn = (* math stuff (n m -- result) *)
    Builtin (fun i -> match pop_n i 2 with
    | Ok (vs, i) -> (match vs with
      | [v1; v2] -> (match v1 with
        | Int i1 -> (match v2 with
          | Int i2 -> Ok (push i (Int (intfn i1 i2)))
          | Float f -> Ok (push i (Float (floatfn f (float_of_int i1)))))
        | Float f -> Ok (push i (Float (floatfn f (get_float v2)))))
      | _ -> Error Unreachable)
    | Error e -> Error e)
  in
  let comp_fn fn = (* boolean comparison functions (v1 v2 -- bool) *)
    Builtin (fun i -> match pop_n i 2 with
      | Ok ([v1; v2], i) -> Ok (push i (value_of_bool (fn v1 v2)))
      | Error e -> Error e
      | _ -> Error Unreachable
    )
  in
  match string with
  (* duplicate a value on the stack (v -- v v) *)
  | "dup" -> Some (Builtin (fun i -> match pop i with
    | Ok (v, i) -> Ok (push (push i v) v)
    | Error e -> Error e))
  (* remove a value from the stack (v --) *)
  | "drop" -> Some (Builtin (fun i -> match pop i with
    | Ok (_, i) -> Ok i
    | Error e -> Error e))
  (* swap the top two elements on the stack ( v1 v2 -- v2 v1 ) *)
  | "swap" -> Some (Builtin (fun i -> match pop_n i 2 with
    | Ok ([v1; v2], i) -> Ok (pushs i [v2; v1])
    | Error e -> Error e
    | _ -> Error Unreachable))
  (* take second element on the stack and duplicate it on the top (v1 v2 -- v1 v2 v1) *)
  | "over" -> Some (Builtin (fun i -> match pop_n i 2 with
    | Ok ([v1; v2], i) -> Ok (pushs i [v1; v2; v1])
    | Error e -> Error e
    | _ -> Error Unreachable))
  (* print a value from the stack (v --) *)
  | "." -> Some (Builtin (fun i -> match pop i with
    | Ok (v, i) -> (match v with
        | Int i -> print_int i
        | Float f -> print_float f); 
      print_char ' '; flush stdout; Ok (set_has_printed i true)
    | Error e -> Error e))
  (* print a new line (--) *)
  | "cr" -> Some (Builtin (fun i -> print_newline (); Ok (set_has_printed i true)))
  (* print the top of the stack as if it was an ascii characer (v --) *)
  | "emit" -> Some (Builtin (fun i -> match pop i with
    | Ok (v, i) -> (match v with
      | Int n -> print_char (char_of_int n); flush stdout; Ok (set_has_printed i true)
      | Float _ -> Error (TypeError v))
    | Error e -> Error e ))
  (* read in a character from stdin (-- c) *)
  | "key" -> Some (Builtin (fun i -> Ok (push i (Int (int_of_char (getchar ()))))))
  (* leave a loop, abusively using errors *)
  | "break" -> Some (Builtin (fun i -> Error (Break i)))
  (* math stuff (n m -- result) *)
  | "+" -> Some (math_fn (+) (+.))
  | "-" -> Some (math_fn (-) (-.))
  | "*" -> Some (math_fn ( * ) ( *. ))
  | "/" -> Some (math_fn (/) (/.))
  | "mod" -> Some (Builtin (fun i -> match pop_n i 2 with
    | Ok ([Int i1; Int i2], i) -> Ok (push i (Int (i1 mod i2)))
    | Ok ([Float _ as v; _], _) -> Error (TypeError (v))
    | Ok ([_; Float _ as v], _) -> Error (TypeError (v))
    | Ok _ -> Error Unreachable
    | Error e -> Error e))
  (* boolean comparison functions (v1 v2 -- bool) *)
  | "=" -> Some (comp_fn equal)
  | "!=" -> Some (comp_fn not_equal)
  | "<" -> Some (comp_fn less_than)
  | ">" -> Some (comp_fn greater_than)
  | "<=" -> Some (comp_fn less_than_equal)
  | ">=" -> Some (comp_fn greater_than_equal)
  (* convert value on top of stack into int ( v -- i ) *)
  | "int" -> Some (Builtin (fun i -> match pop i with
    | Ok (v, i) -> (match v with
      | Int n -> Ok (push i (Int n))
      | Float f -> Ok (push i (Int (int_of_float f))))
    | Error e -> Error e))
  (* convert value on top of stack to float ( v -- f ) *)
  | "float" -> Some (Builtin (fun i -> match pop i with
    | Ok (v, i) -> (match v with
      | Int n -> Ok (push i (Float (float_of_int n)))
      | Float f -> Ok (push i (Float f)))
    | Error e -> Error e))
  (* used for getting the current iteration of a do loop (-- v) *)
  | "i" -> Some (Builtin (fun i -> Ok (push i i.i)))
  (* dereference pointer (address -- val) *)
  | "@" -> Some (Builtin (fun i -> match pop i with
    | Ok (v, i) -> (match v with
      | Int n -> (match Hashtbl.find_opt i.memory n with
        | Some v -> Ok (push i v)
        | None -> Error InaccessibleDereference)
      | Float _ -> Error (TypeError v))
    | Error e -> Error e))
  (* store value at pointer (val address --) *)
  | "!" -> Some (Builtin (fun i -> match pop i with
    | Ok (v, i) -> (match v with
      | Int addr -> (match pop i with
        | Ok (v, i) -> Hashtbl.add i.memory addr v; Ok i
        | Error e -> Error e)
      | Float _ -> Error (TypeError v))
    | Error e -> Error e))
  (* because memory is stored in a hashtable, and all of the hard work is done for us, we really 
  don't need to worry about the size of values. *)
  | "cells" -> Some (Builtin (fun i -> Ok (push i (Int 1))))
  (* allocating memory is a no-op because of hashtables *)
  | "allot" -> Some (Builtin (fun i -> Ok i))
  | _ -> None

(* parse a string. may result in a word or just a state change *)
let parse parser word =
  let cc = parser.comment_count in
  let state = parser.state in
  let words = parser.words in
  let append_word ?s word = 
    let state = match s with
      | Some s -> s
      | None -> state
    in
    match state with
      | [] -> Ok ({ comment_count = cc; state = state; words = words }, Some word)
      | WordDefinition (name, ws) :: state -> (match name with (* Extend the word definition *)
        | Some _ -> Ok ({ comment_count = cc; state = (WordDefinition (name, word :: ws)) :: state; words = words }, None)
        | None -> Error UnnamedWordDefinition)
      | (DoLoop ws) :: state -> Ok ({ comment_count = cc; state = ((DoLoop (word :: ws)) :: state); words = words}, None)
      | (BeginUntilLoop ws) :: state -> Ok ({ comment_count = cc; state = ((BeginUntilLoop (word :: ws)) :: state); words = words}, None)
      | (IfThen ws) :: state -> Ok ({ comment_count = cc; state = ((IfThen (word :: ws)) :: state); words = words}, None)
      | (IfElseThen (tws, fws)) :: state -> Ok ({ comment_count = cc; state = ((IfElseThen (tws, (word :: fws))) :: state); words = words}, None)
      | String _ :: _ -> Error Unreachable
  in
  if (cc != 0) then (* handle comments *)
    match word with
    | "(" -> Ok ({ comment_count = cc + 1; state = state; words = words }, None)
    | ")" -> Ok ({ comment_count = cc - 1; state = state; words = words }, None)
    | _ -> Ok (parser, None)
  else match state with
    (* while constructing a string, append a new word to the string, or catch the end of the string *)
    | String ss :: state -> if (String.ends_with ~suffix:"\"" word)
      then let s = string_of_string_list (List.rev ((String.sub word 0 ((String.length word) - 1)) :: ss)) in
        append_word ~s:state (Builtin (fun i -> print_string s; flush stdout; Ok (set_has_printed i true)))
      else Ok ({ comment_count = cc; state = (String (word :: ss)) :: state; words = words }, None)
    | _ -> 
      match word with
      | "" -> Ok (parser, None)
      | "(" -> Ok ({ comment_count = cc + 1; state = state; words = words }, None)
      | ")" -> Error MisplacedCommentClose
      | ":" -> if (state = []) (* word definitions can only happen on the top level *)
        then Ok ({ comment_count = cc; state = [WordDefinition (None, [])]; words = words }, None)
        else Error MisplacedWordDefinition
      | ";" -> (match state with (* word definitions only end at the very end *)
        | [WordDefinition (Some name, ws)] -> Hashtbl.add words name (List.rev ws); Ok ({ comment_count = cc; state = []; words = words }, None)
        | _ -> Error MisplacedWordDefinitionClose)
      | ".\"" -> Ok ({ comment_count = cc; state = String [] :: state; words = words }, None)
      | "\"" -> Error MisplacedStringClose (* string state is handled above *)
      | "do" -> Ok ({ comment_count = cc; state = (DoLoop []) :: state; words = words }, None)
      | "loop" -> (match state with
        | (DoLoop ws) :: state -> append_word ~s:state (DoLoop (List.rev ws))
        | _ -> Error MisplacedLoop)
      | "begin" -> Ok ({ comment_count = cc; state = (BeginUntilLoop []) :: state; words = words }, None)
      | "until" -> (match state with
        | (BeginUntilLoop ws) :: state -> append_word ~s:state (BeginUntilLoop (List.rev ws))
        | _ -> Error MisplacedUntil)
      | "if" -> Ok ({ comment_count = cc; state = (IfThen []) :: state; words = words }, None)
      | "else" -> (match state with
        | (IfThen ws) :: state -> Ok ({ comment_count = cc; state = (IfElseThen (ws, [])) :: state; words = words }, None)
        | _ -> Error MisplacedElse
      )
      | "then" -> (match state with
        | (IfThen ws) :: state -> append_word ~s:state (IfThen (List.rev ws))
        | (IfElseThen (tws, fws)) :: state -> append_word ~s:state (IfElseThen (List.rev tws, List.rev fws))
        | _ -> Error MisplacedThen)
      | _ -> (* Generate a word from a string *)
        match state with (* Set the name of the word if it is unnamed *)
        | (WordDefinition (None, [])) :: state -> Ok ({ comment_count = cc; state = ((WordDefinition (Some word, [])) :: state); words = words }, None)
        | _ -> match int_of_string_opt word with (* handle int *)
          | Some i -> append_word (Value (Int i))
          | None -> match float_of_string_opt word with (* handle float *)
          | Some f -> append_word (Value (Float f))
          | None -> match builtin_of_string word with (* handle builtins *)
          | Some b -> append_word b
          | None -> match Hashtbl.find_opt words word with (* handle user-defined words *)
          | Some ws -> append_word (Subroutine ws)
          | None -> Error (UnknownWord word) (* idk what is is *)

(* parse a string list *)
let parse_words parser words =
  let rec aux parser acc = function
  | [] -> Ok (parser, acc)
  | w :: ws -> match parse parser w with
    | Ok (parser, word) -> (match word with
      | Some word -> aux parser (word :: acc) ws
      | None -> aux parser acc ws)
    | Error e -> Error e
  in
  match aux parser [] words with
  | Ok (parser, ws) -> Ok (parser, List.rev ws)
  | Error e -> Error e

(* loop that repeats until the top of the stack reports true *)
let rec interpret_begin_until_loop interpreter words =
  match interpret_words interpreter words with
    | Ok i -> (match pop i with
      | Ok (v, i) -> if (bool_of_value v)
        then Ok i
        else interpret_begin_until_loop i words
      | Error e -> Error e)
    | Error Break i -> Ok i
    | Error e -> Error e

(* loop that repeats, where every iteration i is a number that increments by one from low to high *)
and interpret_do_loop interpreter words =
  let rec aux low high interpreter words =
    if (low = high) then Ok interpreter
    else
      match interpret_words ({ stack = interpreter.stack; i = Int low; memory = interpreter.memory; has_printed = interpreter.has_printed }) words with
      | Ok i -> aux (low + 1) high i words
      | Error Break i -> Ok i
      | Error e -> Error e
  in
  match pop_n interpreter 2 with
  | Ok ([high; low], i) -> (match low with
    | Int low -> (match high with
      | Int high -> aux low high i words
      | _ -> Error (TypeError high))
    | _ -> Error (TypeError low))
  | Error e -> Error e
  | _ -> Error Unreachable

and interpret_words interpreter = function
| [] -> Ok interpreter
| w :: ws -> match interpret interpreter w with
  | Ok i -> interpret_words i ws
  | Error e -> Error e

and interpret interpreter word =
  match word with
  | Value v -> Ok (push interpreter v)
  | Builtin b -> (match (b interpreter) with
    | Ok i -> Ok i
    | Error e -> Error e)
  | Subroutine words -> interpret_words interpreter words
  | DoLoop words -> interpret_do_loop interpreter words
  | BeginUntilLoop ws -> interpret_begin_until_loop interpreter ws
  | IfThen ws -> (match pop interpreter with
    | Ok (v, i) -> if (bool_of_value v) then interpret_words i ws else Ok i
    | Error e -> Error e)
  | IfElseThen (tws, fws) -> (match pop interpreter with
    | Ok (v, i) -> if (bool_of_value v) then interpret_words i tws else interpret_words i fws
    | Error e -> Error e)

let rec main parser interpreter = 
  let continue p i = if i.has_printed then print_newline(); main p (set_has_printed i false) in
  if parser.comment_count = 0
  then print_string ">>> "
  else print_string "(comment)>> ";
  let line = read_line () in
  (match parse_words parser (split_string line) with
  | Ok (parser, words) -> (match interpret_words interpreter words with
    | Ok interpreter -> continue parser interpreter (* If everything is OK, we'll end up here *)
    | Error e -> print_string (string_of_interpreter_error e))
  | Error e -> print_string (string_of_parser_error e));
  continue parser interpreter (* If we error, we will end up here *)

let () =
  let parser = { comment_count = 0; state = []; words = Hashtbl.create 5 } in
  let interpreter = { stack = []; i = Int 1; memory = Hashtbl.create 5; has_printed = false } in
  main parser interpreter