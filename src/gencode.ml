open Asttypes
open Ident
open Machine
open Format

let preamble =
  "#include <stdio.h>\n\
   #include <stdbool.h>\n\n"

let ty fmt = function
  | Tbool -> fprintf fmt "bool"
  | Tint -> fprintf fmt "int"
  | Treal -> fprintf fmt "float"

let variable fmt (id,bty) =
  fprintf fmt "%a %a" ty bty fmt_ident id

let variable_decl fmt (id,bty) =
  fprintf fmt "%a;" variable (id,bty)

let instance_decl fmt (id,node_name) =
  fprintf fmt "%s_state %a;" node_name fmt_ident id

let list sep_string pp_item fmt xs : unit =
  pp_print_list
    ~pp_sep:(fun fmt () -> pp_print_string fmt sep_string)
    pp_item
    fmt
    xs

let rec operator fmt op expr_list =
  let pp format = fprintf fmt format in
  match op, expr_list with
  | Op_eq, [e1;e2] -> pp "%a == %a" expr e1 expr e2
  | Op_neq, [e1;e2] -> pp "%a != %a" expr e1 expr e2
  | Op_lt, [e1;e2] -> pp "%a < %a" expr e1 expr e2
  | Op_le, [e1;e2] -> pp "%a <= %a" expr e1 expr e2
  | Op_gt, [e1;e2] -> pp "%a > %a" expr e1 expr e2
  | Op_ge, [e1;e2] -> pp "%a >= %a" expr e1 expr e2
  | Op_add, [e1;e2] -> pp "%a + %a" expr e1 expr e2
  | Op_sub, [e1;e2] -> pp "%a - %a" expr e1 expr e2
  | Op_mul, [e1;e2] -> pp "%a * %a" expr e1 expr e2
  | Op_div, [e1;e2] -> pp "%a / %a" expr e1 expr e2
  | Op_mod, [e1;e2] -> pp "%a %% %a" expr e1 expr e2
  | Op_not, [e] -> pp "!%a" expr e
  | Op_and, [e1;e2] -> pp "%a && %a" expr e1 expr e2
  | Op_or, [e1;e2] -> pp "%a || %a" expr e1 expr e2
  | Op_impl, [e1;e2] -> pp "!(%a) || %a" expr e1 expr e2
  | _ -> assert false

and expr fmt = function
  | ME_local id -> fmt_ident fmt id
  | ME_const c -> print_const fmt c
  | ME_state_var id -> fprintf fmt "self->%a" fmt_ident id
  | ME_op (op, expr_list) ->
      operator fmt op expr_list

let assign_local_after_step fmt (id_loc,result_path) =
  fprintf fmt "%a = %s;" fmt_ident id_loc result_path

let rec instr machine env fmt = function
  | MI_assign_local (id,e) ->
      fprintf fmt "%a = %a;" fmt_ident id expr e
  | MI_assign_state (id,e) ->
      fprintf fmt "self->%a = %a;" fmt_ident id expr e
  | MI_sequence (i1,i2) ->
      fprintf fmt "%a\n%a" (instr machine env) i1 (instr machine env) i2
  | MI_skip -> ()
  | MI_reset id ->
      let node_name = List.assoc id machine.m_instances in
      let _,_,reset_name,_ = List.assoc node_name env in
      fprintf fmt "%s(&self->%a);" reset_name fmt_ident id
  | MI_step (ids, inst_id, arg_list) ->
      let node_name = List.assoc inst_id machine.m_instances in
      let _,step_name,_,called_machine = List.assoc node_name env in
      let result_paths = List.map
        (fun (id,_) ->
          let b = Buffer.create 2048 in
          let strfmt = formatter_of_buffer b in
          fprintf strfmt "self->%a.%a" fmt_ident inst_id fmt_ident id;
          pp_print_flush strfmt ();
          Buffer.contents b
        )
        called_machine.m_step.ms_output
      in
      let assignments = List.combine ids result_paths in
      fprintf fmt "%s(&self->%a, %a);\n\
        %a;"
        step_name
        fmt_ident inst_id
        (list ", " expr) arg_list
        (list "\n" assign_local_after_step) assignments
  | MI_case (cond_id, ift, iff) ->
      fprintf fmt "if(%a) {\n\
          %a\n\
        } else {\n\
          %a\n\
        }\n"
        fmt_ident cond_id
        (instr machine env) ift
        (instr machine env) iff

let struct_decl fmt machine =
  let name = machine.m_name in
  fprintf fmt "typedef struct {\n\
      %a\n\
      %a\n\
    } %s_state;\n"
    (list " " variable_decl) machine.m_mem
    (list " " instance_decl) machine.m_instances
    name

let reset_decl env fmt machine =
  let struct_name,_,f_name,_ = List.assoc machine.m_name env in
  fprintf fmt "void %s(%s* self) {\n\
      %a\n\
    }\n"
    f_name
    struct_name
    (instr machine env) machine.m_reset

let step_decl env fmt machine =
  let step = machine.m_step in
  let struct_name,step_name,_,_ = List.assoc machine.m_name env in
  fprintf fmt "void %s(%s* self%s%a) {\n\
      %a\n\
      %a\n\
    }\n"
    step_name
    struct_name
    (if step.ms_input = [] then "" else ", ")
    (list ", " variable) step.ms_input
    (list " " variable_decl) step.ms_local
    (instr machine env) step.ms_code

let gen_file machines =
  let c_code,_ = List.fold_left
    (fun (acc,env) m ->
      let name = m.m_name in
      let env =
        env @
        [(name, (name ^ "_state", name ^ "_step", name ^ "_reset", m))]
      in
      fprintf str_formatter "%a\n" struct_decl m;
      fprintf str_formatter "%a\n" (reset_decl env) m;
      fprintf str_formatter "%a\n" (step_decl env) m;
      let acc = acc ^ flush_str_formatter () in
      acc,env
    )
    (preamble, [])
    machines
  in
  c_code
