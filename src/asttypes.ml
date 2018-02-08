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
  | Op_if

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
