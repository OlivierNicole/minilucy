open Asttypes
open Ast
module T = Typed_ast
open T

type error =
  | Unbound_ident of T.ident
  | Type_mismatch of ty list * ty
      (* list of possible types, actual type *)

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
        raise (Error (loc, Type_mismatch ([ty1], ty2)));
      { texpr_desc = TE_arrow (te1,te2);
        texpr_type = ty1;
        texpr_loc = loc }
  | PE_pre e ->
      let te = type_expr node_env loc_env e in
      { texpr_desc = TE_pre te;
        texpr_type = te.texpr_type;
        texpr_loc = loc }
  | PE_tuple expr_list ->
      let texpr_list = List.map (type_expr node_env loc_env) expr_list in
      { texpr_desc = TE_tuple texpr_list;
        texpr_type = List.concat @@ List.map (fun e -> e.texpr_type) texpr_list;
        texpr_loc = loc }

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
      (TE_op (o, [te1;te2]), ty1)
    end
  | _ ->
      (* Should not happen assuming parsing is correct *)
      assert false

(* This function checks that:
   * all defined identifiers are declared
   * the types of declared identifiers correspond to the declared output and
     local types.
 *)
let type_node node_env node =
  let add env (id,bty) = Env.add id bty env in
  let loc_env = List.fold_left add Env.empty node.pn_local in
  let loc_env = List.fold_left add loc_env node.pn_input in
  { tn_name = node.pn_name;
    tn_input = node.pn_input;
    tn_output = node.pn_output;
    tn_local = node.pn_local;
    tn_equs = typed_equs;
    tn_loc = node.pn_loc }
