open Asttypes
open Ast
open Format

let list sep_string pp_item fmt xs : unit =
  pp_print_list
    ~pp_sep:(fun fmt () -> pp_print_string fmt sep_string)
    pp_item fmt xs

let rec expr fmt exp =
  match exp.pexpr_desc with
  | PE_const c -> const fmt c
  | PE_ident id -> pp_print_string fmt id
  | PE_op(o,expr_list) ->
      op fmt o expr_list
  | PE_app(f_id, expr_list) ->
      fprintf fmt "%s %a"
        f_id
        (list " " expr)
        expr_list
  | PE_arrow(e1,e2) ->
      fprintf fmt "%a -> %a" expr e1 expr e2
  | PE_fby (c,e2) ->
      fprintf fmt "%a fby %a" const c expr e2
  | PE_tuple expr_list ->
      fprintf fmt "(%a)" (list ", " expr) expr_list
  | PE_when (e, b, (id,_)) ->
      fprintf fmt "%a when %b(%s)" expr e b id
  | PE_merge ((id,_), ift, iff) ->
      fprintf fmt "merge %s (%a) (%a)" id expr ift expr iff
  | PE_if ((id,_), ift, iff) ->
      fprintf fmt "if %s (%a) (%a)" id expr ift expr iff

and const fmt = function
  | Cbool b -> pp_print_bool fmt b
  | Cint i -> pp_print_int fmt i
  | Creal f -> pp_print_float fmt f

and operator fmt o =
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

and op fmt o expr_list =
  (* Distinguish by arity: unary, binary or ternary *)
  match o with
  | Op_not ->
    begin match expr_list with
    | [e] ->
        fprintf fmt "%a %a" operator o expr e
    | _ -> assert false
    end
  | Op_eq | Op_neq | Op_lt | Op_le | Op_gt | Op_ge | Op_add | Op_sub | Op_mul
  | Op_div | Op_mod | Op_and | Op_or | Op_impl ->
    begin match expr_list with
    | [e1;e2] ->
        fprintf fmt "%a %a %a" expr e1 operator o expr e2
    | _ -> assert false
    end

let pattern fmt pat =
  match pat.ppatt_idents with
  | [id] -> fprintf fmt "%s" id
  | ids -> fprintf fmt "(%a)" (list ", " pp_print_string) ids

let equation fmt { peq_patt = pat; peq_expr = exp } =
  fprintf fmt "%a = %a" pattern pat expr exp

let typed_ident fmt (id, ty) =
  fprintf fmt "%s : %a" id Asttypes.print_base_ty ty

let node fmt n =
  fprintf fmt "p_node {\n\
    pn_name = %s;\n\
    pn_input = %a;\n\
    pn_output = %a;\n\
    pn_local = %a;\n\
    pn_equs = \n  %a;\n\
    }"
    n.pn_name
    (list ", " typed_ident) n.pn_input
    (list ", " typed_ident) n.pn_output
    (list ", " typed_ident) n.pn_local
    (list "\n  " equation) n.pn_equs

let type_decl fmt td =
  fprintf fmt "type %s = %a" td.pt_name
    (list " | " pp_print_string) td.pt_constr

let decl fmt = function
  | Node_decl nd -> node fmt nd
  | Type_decl td -> type_decl fmt td

let file fmt decls =
  list "\n\n" decl fmt decls
