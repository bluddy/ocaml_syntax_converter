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

let main () =
  parse_cmd_line ();
  if global_config.input_file = "" then
    Arg.usage param_specs usage_msg
  else
    let ic = open_in global_config.input_file in
    Location.input_name := global_config.input_file;
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf global_config.input_file;
    let ast = Parse.implementation lexbuf in
    Pprintast.structure Format.std_formatter ast

let _ = main ()
