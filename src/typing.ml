open Asttypes
open Ast
module T = Typed_ast
open T

type error =
  | Unbound_ident of T.ident
  | Type_mismatch of ty list * ty
      (* list of possible types, actual type *)
  | Undeclared_ident of T.ident
      (* When an equation defines a identifier that is not declared as a local
       * variable *)
  | Duplicate_node_decl of T.ident
      (* When there are two node declarations with the same name *)
  | Duplicate_local_decl of T.ident
      (* When a local identifier is declared more than once in a node. *)
  | Undefined_local of T.ident

exception Error of location * error

module Env = Map.Make(String)

type env = base_ty Env.t
type node_env = t_node Env.t

let type_const = function
  | Cbool _ -> [Tbool]
  | Cint _ -> [Tint]
  | Creal _ -> [Treal]

let rec type_expr node_env loc_env { pexpr_desc = pdesc; pexpr_loc = loc } =
  match pdesc with
  | PE_const c ->
      { texpr_desc = TE_const c; texpr_type = type_const c; texpr_loc = loc }
  | PE_ident id ->
    begin try
      let ty = Env.find id loc_env in
      { texpr_desc = TE_ident id; texpr_type = ty; texpr_loc = loc }
    with Not_found ->
      raise (Error (loc, Unbound_ident id))
    end
  | PE_op (o, expr_list) ->
      let desc,ty = type_op node_env loc_env o expr_list in
      { texpr_desc = desc; texpr_type = ty; texpr_loc = loc }
  | PE_app (node_id, expr_list) ->
    begin try
      let node = Env.find node_id node_env in
      let texpr_list = List.map (type_expr node_env loc_env) expr_list in
      (* Flatten tuples in the argument types *)
      let args_ty =
        List.concat @@ List.map (fun e -> e.texpr_type) texpr_list
      in
      let input_ty = List.map snd node.tn_input in
      (* Check identity between input types and argument types *)
      if args_ty <> input_ty then
        raise (Error (loc, Type_mismatch ([input_ty], args_ty)));
      { texpr_desc = TE_app (node_id, texpr_list);
        texpr_type = List.map snd node.tn_output;
        texpr_loc = loc }
    with Not_found ->
      raise (Error (loc, Unbound_ident node_id))
    end
  | PE_arrow (e1,e2) ->
      let {texpr_type = ty1} as te1 = type_expr node_env loc_env e1 in
      let {texpr_type = ty2} as te2 = type_expr node_env loc_env e2 in
      if ty1 <> ty2 then
        raise (Error (te2.texpr_loc, Type_mismatch ([ty1], ty2)));
      { texpr_desc = TE_arrow (te1,te2);
        texpr_type = ty1;
        texpr_loc = loc }
  | PE_fby (const,e2) ->
      let ty_const = type_const const in
      let {texpr_type = ty2} as te2 = type_expr node_env loc_env e2 in
      if ty_const <> ty2 then
        raise (Error (te2.texpr_loc, Type_mismatch ([ty_const], ty2)));
      { texpr_desc = TE_fby (const,te2);
        texpr_type = ty_const;
        texpr_loc = loc }
  | PE_tuple expr_list ->
      let texpr_list = List.map (type_expr node_env loc_env) expr_list in
      { texpr_desc = TE_tuple texpr_list;
        texpr_type = List.concat @@ List.map (fun e -> e.texpr_type) texpr_list;
        texpr_loc = loc }
  | PE_when (e, (var_id, var_id_loc)) ->
    begin try
      let ty_var = Env.find var_id loc_env in
      if ty_var <> [Tbool] then
        raise (Error (var_id_loc, Type_mismatch ([[Tbool]], ty_var)));
      let te = type_expr node_env loc_env e in
      { texpr_desc = TE_when (te, (var_id, var_id_loc));
        texpr_type = te.texpr_type;
        texpr_loc = loc }
    with Not_found ->
      raise (Error (var_id_loc, Undeclared_ident var_id))
    end
  | PE_merge ((var_id, var_id_loc), ift, iff) ->
    begin try
      let ty_var = Env.find var_id loc_env in
      if ty_var <> [Tbool] then
        raise (Error (var_id_loc, Type_mismatch ([[Tbool]], ty_var)));
      let tift = type_expr node_env loc_env ift in
      let tiff = type_expr node_env loc_env iff in
      if tift.texpr_type <> tiff.texpr_type then
        raise (Error (tiff.texpr_loc,
          Type_mismatch ([tift.texpr_type], tiff.texpr_type)));
      { texpr_desc = TE_merge ((var_id, var_id_loc), tift, tiff);
        texpr_type = tift.texpr_type;
        texpr_loc = loc }
    with Not_found ->
      raise (Error (var_id_loc, Undeclared_ident var_id))
    end

and type_op node_env loc_env o expr_list =
  match o, expr_list with
  | Op_if, [cond;ift;iff] ->
      let t_cond = type_expr node_env loc_env cond in
      if t_cond.texpr_type <> [Tbool] then
        raise (Error (cond.pexpr_loc, Type_mismatch ([[Tbool]],
          t_cond.texpr_type)));
      let t_e1 = type_expr node_env loc_env ift in
      let t_e2 = type_expr node_env loc_env iff in
      if t_e1.texpr_type <> t_e2.texpr_type then
        raise (Error (iff.pexpr_loc, Type_mismatch ([t_e1.texpr_type],
          t_e2.texpr_type)));
      (TE_op (Op_if, [t_cond; t_e1; t_e2]), t_e1.texpr_type)
  | Op_not, [e] ->
      let { texpr_type = ty } as t_e = type_expr node_env loc_env e in
      if ty <> [Tbool] then
        raise (Error (e.pexpr_loc, Type_mismatch ([[Tbool]], ty)));
      (TE_op (Op_not, [t_e]), [Tbool])
  | _, [e1;e2] ->
    begin
      (* First, we check the first operand's type. *)
      let { texpr_type = ty1 } as te1 = type_expr node_env loc_env e1 in
      (* List of admissible types for both operands. Empty list means any type.
       * *)
      let expected_ty1 = match o with
      | Op_eq | Op_neq -> []
      | Op_lt | Op_le | Op_gt | Op_ge | Op_add | Op_sub | Op_mul | Op_div ->
          [[Tint];[Treal]]
      | Op_mod -> [[Tint]]
      | Op_and | Op_or | Op_impl -> [[Tbool]]
      | _ -> (* handled above *) assert false
      in
      begin match expected_ty1 with
      | [] -> ()
      | _ ->
          if not (List.mem ty1 expected_ty1) then
            raise (Error (e1.pexpr_loc, Type_mismatch (expected_ty1, ty1)));
      end;
      (* Now checking the equality of type between the operands. *)
      let { texpr_type = ty2 } as te2 = type_expr node_env loc_env e2 in
      if ty1 <> ty2 then
        raise (Error (e2.pexpr_loc, Type_mismatch ([ty1], ty2)));
      let result_type = match o with
      | Op_eq | Op_neq | Op_lt | Op_le | Op_gt | Op_ge -> [Tbool]
      | _ -> ty1
      in
      (TE_op (o, [te1;te2]), result_type)
    end
  | _ ->
      (* Should not happen assuming parsing is correct *)
      assert false

let add_idents ids env =
  let add env (id,bty) = Env.add id [bty] env in
  List.fold_left add env ids

let t_patt_of_p_patt { ppatt_desc = desc; ppatt_loc = loc } =
  let tdesc = match desc with
  | PP_ident id -> TP_ident id
  | PP_tuple ids -> TP_tuple ids
  in
  { tpatt_desc = tdesc; tpatt_loc = loc }

(* [type_equation node_env loc_env equ] checks that the identifiers defined by
 * [equ] are present in [loc_env], that they are associated with the right
 * type, and returns the typed equations if there is no error.
 *)
let type_equation node_env loc_env { peq_patt = pat; peq_expr = e } =
  match pat.ppatt_desc with
  | PP_ident id ->
    begin try
      let expected_ty = Env.find id loc_env in
      let te = type_expr node_env loc_env e in
      if te.texpr_type <> expected_ty then
        raise (Error (te.texpr_loc,
          Type_mismatch ([expected_ty], te.texpr_type)));
      { teq_patt = t_patt_of_p_patt pat;
        teq_expr = te }
    with Not_found ->
      raise (Error (pat.ppatt_loc, Undeclared_ident id))
    end
  | PP_tuple ids ->
    begin
      (* Expected type is the flattened tuple of all declared types *)
      let expected_ty = List.concat @@ List.map
        (fun id ->
          try Env.find id loc_env
          with Not_found ->
            raise (Error (pat.ppatt_loc, Undeclared_ident id))
        )
        ids
      in
      let te = type_expr node_env loc_env e in
      if te.texpr_type <> expected_ty then
        raise (Error (pat.ppatt_loc,
          Type_mismatch ([expected_ty], te.texpr_type)));
      { teq_patt = t_patt_of_p_patt pat;
        teq_expr = te }
    end

(* Idents defined by an equation *)
let defined_idents { teq_patt = pat } =
  match pat.tpatt_desc with
  | TP_ident id -> [id]
  | TP_tuple ids -> ids

let first_duplicate l =
  let rec aux acc = function
  | [] -> None
  | x :: xs ->
      if List.mem x acc then Some x
      else aux (x :: acc) xs
  in
  aux [] l

module StringSet = Set.Make(String)

(* This function checks that:
   * no node of this name is in scope
   * all defined identifiers are declared
   * all declared identifiers are defined
   * every identifier is defined only once
   * the types of declared identifiers correspond to the declared output and
     local types.
 *)
let type_node node_env node =
  if Env.mem node.pn_name node_env then
    raise (Error (node.pn_loc, Duplicate_node_decl node.pn_name));
  (* Check that all identifiers are declared once *)
  let declared = node.pn_local @ node.pn_output @ node.pn_input in
  begin match first_duplicate @@ List.map fst declared with
  | None -> ()
  | Some id ->
      raise (Error (node.pn_loc, Duplicate_local_decl id))
  end;
  (* Add all declared identifiers to the environment *)
  let loc_env = add_idents declared Env.empty in
  (* For every equation, check that the defined identifiers are declared and
   * that the types match. *)
  let typed_equs = List.map
    (type_equation node_env loc_env)
    node.pn_equs
  in
  (* Check that there all declared identifiers are defined by an equation. *)
  let to_define = List.map fst @@ node.pn_output @ node.pn_local in
  let defined = List.concat @@ List.map defined_idents typed_equs in
  let undef =
    StringSet.diff (StringSet.of_list to_define) (StringSet.of_list defined)
  in
  if not (StringSet.is_empty undef) then
    raise (Error (node.pn_loc, Undefined_local (StringSet.choose undef)));
  { tn_name = node.pn_name;
    tn_input = node.pn_input;
    tn_output = node.pn_output;
    tn_local = node.pn_local;
    tn_equs = typed_equs;
    tn_loc = node.pn_loc }

let type_file file =
  let final_env = List.fold_left
    (fun node_env node_or_type ->
      match node_or_type with
      | Node_decl node ->
          let tnode = type_node node_env node in
          Env.add tnode.tn_name tnode node_env
      | Type_decl _ ->
          node_env
    )
    Env.empty
    file
  in
  List.map snd @@ Env.bindings final_env

open Format

let report_error fmt = function
  | Unbound_ident id ->
      fprintf fmt "Unbound identifier '%s'." id
  | Type_mismatch (possible_types, ty) ->
      fprintf fmt "Expected type %a but got %a."
        (pp_print_list
          ~pp_sep:(fun fmt () -> pp_print_string fmt " or ")
          print_ty)
        possible_types
        print_ty
        ty
  | Undeclared_ident id ->
      fprintf fmt
        "This equation defines identifier '%s', which was not declared." id
  | Duplicate_node_decl id ->
      fprintf fmt "There is already a node named '%s'." id
  | Duplicate_local_decl id ->
      fprintf fmt "Identifier '%s' was already declared in this node." id
  | Undefined_local id ->
      fprintf fmt "Identifier '%s' was declared in the node, but not defined."
        id
