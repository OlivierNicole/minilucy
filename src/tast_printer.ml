open Asttypes
open Typed_ast
open Format

let list pp_item fmt xs : unit =
  pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt ", ") pp_item fmt xs

let rec expr fmt exp =
  match exp.texpr_desc with
  | TE_const c -> const fmt c
  | TE_ident id -> pp_print_string fmt id
  | TE_op(o,expr_list) ->
      op fmt o expr_list
  | TE_app(f_id, expr_list) ->
      fprintf fmt "%s %a"
        f_id
        (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt " ") expr)
        expr_list
  | TE_arrow(e1,e2) ->
      fprintf fmt "%a -> %a" expr e1 expr e2
  | TE_pre e ->
      fprintf fmt "pre %a" expr e
  | TE_tuple expr_list ->
      fprintf fmt "(%a)" (list expr) expr_list

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
  | Op_if -> assert false (* ternary operator, not easily representable *)

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
  | Op_if ->
    begin match expr_list with
    | [cond;ift;iff] ->
        fprintf fmt "if %a then %a else %a" expr cond expr ift expr iff
    | _ -> assert false
    end

let pattern fmt pat =
  match pat.tpatt_desc with
  | TP_ident id -> fprintf fmt "%s" id
  | TP_tuple ids -> fprintf fmt "(%a)" (list pp_print_string) ids

let equation fmt { teq_patt = pat; teq_expr = exp } =
  fprintf fmt "%a = %a" pattern pat expr exp

let typed_ident fmt (id, ty) =
  fprintf fmt "%s : %a" id Asttypes.print_base_ty ty

let node fmt n =
  fprintf fmt "p_node {\n\
    tn_name = %s;\n\
    tn_input = %a;\n\
    tn_output = %a;\n\
    tn_local = %a;\n\
    tn_equs = \n  %a;\n\
    }"
    n.tn_name
    (list typed_ident) n.tn_input
    (list typed_ident) n.tn_output
    (list typed_ident) n.tn_local
    (pp_print_list
      ~pp_sep:(fun fmt () -> pp_print_string fmt "\n  ") equation)
      n.tn_equs

let file fmt nodes =
  pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt "\n\n")
    node fmt nodes
