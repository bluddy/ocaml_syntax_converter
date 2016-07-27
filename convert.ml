
type state =
  | Normal
  | String
  | Comment of int (* level *)
  | QuotedString of string (* delimiter *)

let changes = [
  Str.regexp "\\bmatch\\b", "begin match";
  Str.regexp "\\bfunction\\b", "begin function";
  Str.regexp "\\btry\\b", "begin try";
  Str.regexp "\\bthen\\b", "then begin";
  Str.regexp "\\belse\\b", "end else begin"
]

(* pattern finding any context change characters *)
let r_context_change = Str.regexp "(\\*\\|{[^|]*|\\|\""
let r_comment_change = Str.regexp "(\\*\\|\\*)"
let r_quotes = Str.regexp "\""
(* to extract the quote in the quoted string *)
let r_quoted_string = Str.regexp "^{\\([^|]*\\)|"

let convert s =
  let buf = Buffer.create (String.length s) in
  let rec loop s state =
    if s = "" then ()
    else
      match state with
      | Normal ->
          (* search for next context changer *)
          let change_pos, change_pos_end, matched_string =
            try
              let cpos = Str.search_forward r_context_change s 0 in
              cpos, Str.match_end (), Str.matched_string s
            with Not_found ->
              let l = String.length s in
              l, l, ""
          in
          let s_before = Str.string_before s change_pos in
          (* perform changes *)
          let s_before =
            List.fold_left (fun acc (regex, replace) ->
                Str.global_replace regex replace acc)
              s_before
              changes
          in
          Buffer.add_string buf s_before;
          Buffer.add_string buf matched_string;
          let s_after = Str.string_after s change_pos_end in
          (* decide on the new state *)
          if s_after = "" then () else
          let new_state = begin match s.[change_pos] with
            | '(' -> Comment 1
            | '"' -> String
            | '{' when Str.string_match r_quoted_string matched_string 0 ->
                let quote = Str.matched_group 1 matched_string in
                QuotedString quote
            | _ -> assert false
            end
          in
          loop s_after new_state

      | Comment level ->
          (* look for a deeper comment or end of comment. Skip comment open *)
          let change_pos =
            try
              Str.search_forward r_comment_change s 0
            with Not_found ->
              failwith "Ill-formed comment"
          in
          let change_pos_end = Str.match_end () in
          Buffer.add_substring buf s 0 change_pos_end;
          let s_after = Str.string_after s change_pos_end in
          if s.[change_pos] = '(' then
            (* open comment *)
            loop s_after (Comment(level + 1))
          else
            (* close comment *)
            if level > 1 then 
              loop s_after (Comment(level - 1))
            else
              loop s_after Normal

      | String ->
          (* get position after quote *)
          let end_pos = Str.search_forward r_quotes s 1 + 1 in
          Buffer.add_substring buf s 0 end_pos;
          loop (Str.string_after s end_pos) Normal

      | QuotedString quote ->
          let r_end = Str.regexp (Printf.sprintf "|%s}" quote) in
          ignore(Str.search_forward r_end s 2);
          (* get the pos of next char after string termination *)
          let end_pos = Str.match_end () in
          Buffer.add_substring buf s 0 end_pos;
          loop (Str.string_after s end_pos) Normal
  in
  loop s Normal;
  Buffer.contents buf

let read_file f =
  let buf = Buffer.create 1000 in
  let ic = open_in f in
  let rec loop () =
    try
      Buffer.add_char buf (input_char ic);
      loop ()
    with End_of_file -> ()
  in
  loop ();
  Buffer.contents buf

let main () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s ocaml_file\n" Sys.executable_name
  else
    let s = read_file Sys.argv.(1) in
    print_string (convert s)
    
let _ = main ()




