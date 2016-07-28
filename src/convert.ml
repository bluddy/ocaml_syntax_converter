(* Utility to convert between OCaml syntax forms *)
type syntax =
  | Safe_syntax_403
  | Ocaml_403

type config = {
  mutable input_file: string;
  mutable input_syntax: syntax;
  mutable output_syntax: syntax;
}

let global_config = {
  input_file = "";
  input_syntax = Safe_syntax_403;
  output_syntax = Ocaml_403;
}

let parse_syntax = function
  | "safe" -> Safe_syntax_403
  | "403"  -> Ocaml_403
  | _      -> raise @@ Arg.Bad("Invalid syntax choice")

let param_specs = [
  "-i", Arg.String (fun s -> global_config.input_syntax <- parse_syntax s),
    "Output syntax: 403 (default) | safe";
  "-l", Arg.String (fun s -> global_config.output_syntax <- parse_syntax s),
    "Input syntax: 403 | safe (default)";
]

let usage_msg = Printf.sprintf "%s [options] input_file" Sys.executable_name

let parse_cmd_line () =
  Arg.parse param_specs (fun f -> global_config.input_file <- f) usage_msg

let get_parse_fns = function
  | Safe_syntax_403 -> Parse_403_safe.implementation, Parse_403_safe.interface
  | Ocaml_403 -> Parse_403.implementation, Parse_403.interface

let get_print_fns = function
  | Safe_syntax_403 -> Pprintast_403_safe.structure, Pprintast_403_safe.signature
  | Ocaml_403 -> Pprintast_403.structure, Pprintast_403.signature

let main () =
  parse_cmd_line ();
  if global_config.input_file = "" then
    Arg.usage param_specs usage_msg
  else
    let file = global_config.input_file in
    let ic = open_in global_config.input_file in
    Location.input_name := global_config.input_file;
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf global_config.input_file;
    (* choose correct parse functions *)
    let parse_impl, parse_interface =
      get_parse_fns global_config.input_syntax in
    let print_impl, print_interface =
      get_print_fns global_config.output_syntax in
    if Filename.check_suffix file ".ml" then
      let ast = parse_impl lexbuf in
      print_impl Format.std_formatter ast
    else if Filename.check_suffix file ".mli" then 
      let ast = parse_interface lexbuf in
      print_interface Format.std_formatter ast
    else
      prerr_string "File does not have OCaml extensions"

let _ = main ()
