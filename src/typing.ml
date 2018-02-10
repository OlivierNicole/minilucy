open Asttypes
open Ast
module T = Typed_ast
open T
open Ident

type error =
  | Unbound_ident of string
  | Type_mismatch of ty list * ty
      (* list of possible types, actual type *)
  | Undeclared_ident of string
      (* When an equation defines a identifier that is not declared as a local
       * variable *)
  | Duplicate_node_decl of string
      (* When there are two node declarations with the same name *)
  | Duplicate_local_decl of string
      (* When a local identifier is declared more than once in a node. *)
  | Undefined_local of ident

exception Error of location * error

(* For expressions generated while desugaring *)
let dummy_loc = Lexing.(dummy_pos, dummy_pos)

module Env = Map.Make(String)

type env = (base_ty * ident) Env.t
type node_env = t_node Env.t

let type_const = function
  | Cbool _ -> [Tbool]
  | Cint _ -> [Tint]
  | Creal _ -> [Treal]

let rec type_expr node_env loc_env { pexpr_desc = pdesc; pexpr_loc = loc } =
  let mkexpr desc ty =
    { texpr_desc = desc;
      texpr_type = ty;
      texpr_clock = [Clock.Ck_base];
      texpr_loc = loc }
  in
  match pdesc with
  | PE_const c ->
      mkexpr (TE_const c) (type_const c)
  | PE_ident id ->
    begin try
      let ty,lid = Env.find id loc_env in
      mkexpr (TE_ident lid) ty
    with Not_found ->
      raise (Error (loc, Unbound_ident id))
    end
  | PE_op (o, expr_list) ->
      let desc,ty = type_op node_env loc_env o expr_list in
      mkexpr desc ty
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
      mkexpr (TE_app (node_id, texpr_list)) (List.map snd node.tn_output)
    with Not_found ->
      raise (Error (loc, Unbound_ident node_id))
    end
  | PE_arrow (e1,e2) ->
      let {texpr_type = ty1} as te1 = type_expr node_env loc_env e1 in
      let {texpr_type = ty2} as te2 = type_expr node_env loc_env e2 in
      if ty1 <> ty2 then
        raise (Error (te2.texpr_loc, Type_mismatch ([ty1], ty2)));
      mkexpr (TE_arrow (te1,te2)) ty1
  | PE_fby (const,e2) ->
      let ty_const = type_const const in
      let {texpr_type = ty2} as te2 = type_expr node_env loc_env e2 in
      if ty_const <> ty2 then
        raise (Error (te2.texpr_loc, Type_mismatch ([ty_const], ty2)));
      mkexpr (TE_fby (const,te2)) ty_const
  | PE_tuple expr_list ->
      let texpr_list = List.map (type_expr node_env loc_env) expr_list in
      mkexpr
        (TE_tuple texpr_list)
        (List.concat @@ List.map (fun e -> e.texpr_type) texpr_list)
  | PE_when (e, b, (var_id, var_id_loc)) ->
    begin try
      let ty_var,lid = Env.find var_id loc_env in
      if ty_var <> [Tbool] then
        raise (Error (var_id_loc, Type_mismatch ([[Tbool]], ty_var)));
      let te = type_expr node_env loc_env e in
      mkexpr (TE_when (te, b, (lid, var_id_loc))) te.texpr_type
    with Not_found ->
      raise (Error (var_id_loc, Undeclared_ident var_id))
    end
  | PE_merge ((var_id, var_id_loc), ift, iff) ->
    begin try
      let ty_var,lid = Env.find var_id loc_env in
      if ty_var <> [Tbool] then
        raise (Error (var_id_loc, Type_mismatch ([[Tbool]], ty_var)));
      let tift = type_expr node_env loc_env ift in
      let tiff = type_expr node_env loc_env iff in
      if tift.texpr_type <> tiff.texpr_type then
        raise (Error (tiff.texpr_loc,
          Type_mismatch ([tift.texpr_type], tiff.texpr_type)));
      mkexpr (TE_merge ((lid, var_id_loc), tift, tiff)) tift.texpr_type
    with Not_found ->
      raise (Error (var_id_loc, Undeclared_ident var_id))
    end
  | PE_if (cond_id_loc, ift, iff) ->
      (* Desugar to merge and when *)
      let ift' = { ift with pexpr_desc =
        PE_when (ift, true, cond_id_loc) }
      in
      let iff' = { iff with pexpr_desc =
        PE_when (iff, false, cond_id_loc) }
      in
      let desc =
        PE_merge (cond_id_loc, ift', iff')
      in
      type_expr node_env loc_env { pexpr_desc = desc; pexpr_loc = loc }

and type_op node_env loc_env o expr_list =
  match o, expr_list with
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
  let add env (id,bty) = Env.add id ([bty], fresh_id id) env in
  List.fold_left add env ids

let t_patt_of_p_patt env { ppatt_idents = ids; ppatt_loc = loc } =
  let t_ids = List.map (fun id -> snd @@ Env.find id env) ids in
  { tpatt_idents = t_ids; tpatt_loc = loc }

(* [type_equation node_env loc_env equ] checks that the identifiers defined by
 * [equ] are present in [loc_env], that they are associated with the right
 * type, and returns the typed equations if there is no error.
 *)
let type_equation node_env loc_env { peq_patt = pat; peq_expr = e } =
  (* Expected type is the flattened tuple of all declared types *)
  let tys,ids = List.split @@ List.map
    (fun id ->
      try Env.find id loc_env
      with Not_found ->
        raise (Error (pat.ppatt_loc, Undeclared_ident id))
    )
    pat.ppatt_idents
  in
  let expected_ty = List.concat @@ tys in
  let te = type_expr node_env loc_env e in
  if te.texpr_type <> expected_ty then
    raise (Error (pat.ppatt_loc,
      Type_mismatch ([expected_ty], te.texpr_type)));
  { teq_patt = t_patt_of_p_patt loc_env pat;
    teq_expr = te }

let first_duplicate l =
  let rec aux acc = function
  | [] -> None
  | x :: xs ->
      if List.mem x acc then Some x
      else aux (x :: acc) xs
  in
  aux [] l

module IdentSet = Set.Make(IdentOrd)

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
  let str_to_idents l =
    List.map (fun (str,bty) -> (snd (Env.find str loc_env), bty)) l
  in
  let inputs = str_to_idents node.pn_input in
  let outputs = str_to_idents node.pn_output in
  let locals = str_to_idents node.pn_local in
  (* For every equation, check that the defined identifiers are declared and
   * that the types match. *)
  let typed_equs = List.map
    (type_equation node_env loc_env)
    node.pn_equs
  in
  (* Check that there all declared identifiers are defined by an equation. *)
  let to_define = List.map fst @@ outputs @ locals in
  let defined = List.concat @@
    List.map (fun eq -> eq.teq_patt.tpatt_idents) typed_equs
  in
  let undef =
    IdentSet.diff (IdentSet.of_list to_define) (IdentSet.of_list defined)
  in
  if not (IdentSet.is_empty undef) then
    raise (Error (node.pn_loc, Undefined_local (IdentSet.choose undef)));
  { tn_name = node.pn_name;
    tn_input = inputs;
    tn_output = outputs;
    tn_local = locals;
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
      fprintf fmt "Identifier '%a' was declared in the node, but not defined."
        fmt_ident id
