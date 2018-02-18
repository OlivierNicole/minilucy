
(* Programme principal *)

open Format
open Lexing
open Lexer
open Parser
open Ast
open Typed_ast
open Scheduling

let usage = "usage: "^Sys.argv.(0)^" file.lus"

let spec = []

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".lus") then
      raise (Arg.Bad "no .lus extension");
    file := Some s
  in
  let cpt = ref 0 in
  let set s =
    incr cpt;
    match !cpt with
    | 1 -> set_file s
    | _ -> raise (Arg.Bad "Too many arguments")
  in
  Arg.parse spec set usage;
  (match !file with Some f -> f | None -> Arg.usage spec usage; exit 1)

let report_loc (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let f = Parser.file Lexer.token lb in
    close_in c;
    let tf = Typing.type_file f in
    Format.printf "Typed tree:\n%a\n\n" Tast_printer.file tf;
    Format.printf "Clocking...\n%!";
    Clocking.clock_file tf;
    Format.printf "Clocking succeeded!\n\n%!";
    let scheduled_f = Scheduling.schedule_file tf in
    Format.printf "Scheduled tree:\n%a\n\n" Tast_printer.file scheduled_f;
    let norm_f = Normalization.normalize_file scheduled_f in
    Format.printf "Normalized tree:\n%a\n" Tast_printer.file norm_f;
    let machines = Translate.transl_file norm_f in
    Format.printf "Object language:\n%a\n" Machine_printer.file machines;
    let code = Gencode.gen_file machines in
    let out_c_file = open_out "out.c" in
    let fmt = Format.formatter_of_out_channel out_c_file in
    Format.fprintf fmt "%s\n" code;
    Format.pp_print_flush fmt ();
    close_out out_c_file;
    Format.printf "C code written to out.c.\n"
  with
    | Lexical_error s ->
	report_loc (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "lexical error: %s\n@." s;
	exit 1
    | Parsing.Parse_error ->
	report_loc (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "syntax error\n@.";
	exit 1
    | Typing.Error (loc, err) ->
        report_loc (fst loc, snd loc);
        Typing.report_error Format.err_formatter err;
        eprintf "\n";
        exit 1
    | Clocking.Error (loc, err) ->
        report_loc (fst loc, snd loc);
        Clocking.report_error Format.err_formatter err;
        eprintf "\n";
        exit 1
    | e ->
        eprintf "Fatal: %s\n@." (Printexc.to_string e);
        exit 2
