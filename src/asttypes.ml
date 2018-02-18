type location = Lexing.position * Lexing.position

type base_ty =
  | Tbool
  | Tint
  | Treal

type ty = base_ty list

type const =
  | Cbool of bool
  | Cint of int
  | Creal of float

type op =
  | Op_eq | Op_neq | Op_lt | Op_le | Op_gt | Op_ge
  | Op_add | Op_sub | Op_mul | Op_div | Op_mod
  | Op_not
  | Op_and | Op_or | Op_impl

open Format

let print_base_ty fmt = function
  | Tbool -> pp_print_string fmt "bool"
  | Tint -> pp_print_string fmt "int"
  | Treal -> pp_print_string fmt "real"

let print_ty fmt = function
  | [] -> assert false
  | [bty] -> print_base_ty fmt bty
  | bty_list ->
      pp_print_list
        ~pp_sep:(fun fmt () -> pp_print_string fmt " * ")
        print_base_ty
        fmt
        bty_list

let print_const fmt = function
  | Cbool b -> pp_print_bool fmt b
  | Cint i -> pp_print_int fmt i
  | Creal f -> pp_print_float fmt f

let print_op fmt o =
  pp_print_string fmt @@ match o with
  | Op_eq -> "="
  | Op_neq -> "<>"
  | Op_lt -> "<"
  | Op_le -> "<="
  | Op_gt -> ">"
  | Op_ge -> ">="
  | Op_add -> "+"
  | Op_sub -> "-"
  | Op_mul -> "*"
  | Op_div -> "/"
  | Op_mod -> "mod"
  | Op_not -> "not"
  | Op_and -> "and"
  | Op_or -> "or"
  | Op_impl -> "=>"
