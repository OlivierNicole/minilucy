type location = Lexing.position * Lexing.position

type base_ty =
  | Tbool
  | Tint
  | Treal

type ty = base_ty list

val print_base_ty : Format.formatter -> base_ty -> unit
val print_ty : Format.formatter -> ty -> unit

type const =
  | Cbool of bool
  | Cint of int
  | Creal of float

val print_const : Format.formatter -> const -> unit

type op =
  | Op_eq | Op_neq | Op_lt | Op_le | Op_gt | Op_ge
  | Op_add | Op_sub | Op_mul | Op_div | Op_mod
  | Op_not
  | Op_and | Op_or | Op_impl

val print_op : Format.formatter -> op -> unit
