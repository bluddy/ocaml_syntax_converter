
let main () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s ocaml_file\n" Sys.executable_name
  else
    let file = Sys.argv.(1) in
    let ic = open_in file in
    Location.input_name := file;
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf file;
    let ast = Parse.implementation lexbuf in
    Pprintast.structure Format.std_formatter ast

let _ = main ()




