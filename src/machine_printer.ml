open Asttypes
open Ident
open Format
open Machine

let list sep_string pp_item fmt xs : unit =
  pp_print_list
    ~pp_sep:(fun fmt () -> pp_print_string fmt sep_string)
    pp_item
    fmt
    xs

let const fmt = function
  | Cbool b -> pp_print_bool fmt b
  | Cint i -> pp_print_int fmt i
  | Creal f -> pp_print_float fmt f

let rec expr fmt = function
  | ME_local id -> fmt_ident fmt id
  | ME_state_var id -> fmt_ident fmt id
  | ME_const c -> print_const fmt c
  | ME_op (o, expr_list) ->
      fprintf fmt "op%a (%a)" print_op o (list ", " expr) expr_list
  | ME_tuple expr_list ->
      fprintf fmt "(%a)" (list ", " expr) expr_list

let rec instr fmt = function
  | MI_assign_local (id, e) ->
      fprintf fmt "%a = %a" fmt_ident id expr e
  | MI_assign_state (id, e) ->
      fprintf fmt "state(%a) = %a" fmt_ident id expr e
  | MI_sequence (i1,i2) ->
      fprintf fmt "%a;\n%a" instr i1 instr i2
  | MI_skip -> fprintf fmt "skip"
  | MI_reset id -> fprintf fmt "reset %a" fmt_ident id
  | MI_step (ids, inst, expr_list) ->
      fprintf fmt "(%a) = %a.step(%a)"
        (list ", " fmt_ident) ids
        fmt_ident inst
        (list ", " expr) expr_list
  | MI_case (id, ift, iff) ->
      fprintf fmt "case(%a):\ntrue:\n%a\nfalse:\n%a"
        fmt_ident id instr ift instr iff

let typed_ident fmt (id, ty) =
  fprintf fmt "%a : %a" fmt_ident id Asttypes.print_base_ty ty

let id_string fmt (id, str) =
  fprintf fmt "%a : %a" fmt_ident id pp_print_string str

let machine fmt m =
  fprintf fmt "machine %s =\n\
    memory %a;\n\
    instances %a;\n\
    reset () =\n\
    %a\n\
    step(%a) returns (%a) =\n\
    %a\n"
  m.m_name
  (list ", " typed_ident) m.m_mem
  (list ", " id_string) m.m_instances
  instr m.m_reset
  (list ", " typed_ident) m.m_step.ms_input
  (list ", " typed_ident) m.m_step.ms_output
  instr m.m_step.ms_code

let file fmt machines =
  list "\n" machine fmt machines
