open Ast
module T = Typed_ast
open T

type error =
  | Unbound_ident of T.ident
  | Type_mismatch of base_ty list * base_ty
      (* list of possible types, actual type *)

exception Error of location * error

module Env = Map.Make(String)

type env = base_ty Env.t
type node_env = t_node Env.t

let type_const = function
  | Cbool -> Tbool
  | Cint -> Tint
  | Creal -> Treal

let rec type_expr node_env tenv { pexpr_desc = pdesc; pexpr_loc = loc } =
  match pdesc with
  | PE_const c ->
      { texpr_desc = TE_const c; texpr_type = type_const c; texpr_loc = loc }
  | PE_ident id ->
    begin try
      let ty = Env.find id env in
      { texpr_desc = TE_ident id; texpr_type = ty; texpr_loc = loc }
    with Not_found ->
      raise (Error (pexpr_loc, Unbound_ident id))
    end
  | PE_op (o, expr_list) ->
      let desc,ty = type_op o e1 e2 in
      { texpr_desc = desc; texpr_type = ty; texpr_loc = loc }
  | PE_app (node_id, expr_list) ->


and type_op o expr_list =
  match o with
  | Op_if ->
      (* This matching is safe assuming parsing is correct *)
      let [cond;ift;iff] = expr_list in
      let ty_cond = type_expr cond in
      if snd ty_cond <> Tbool then
        raise (Error (cond.pexpr_loc, Type_mismatch ([Tbool], snd ty_cond)));
      let ty1 = type_expr ift in
      let ty2 = type_expr iff in
      if snd ty1 <> snd ty2 then
        raise (Error (iff.pexpr_loc, Type_mismatch ([snd ty1], snd ty2)));
      (TE_op (Op_if, [fst ty_cond; fst ty1; fst ty2]), snd ty1)
  | Op_not ->
      (* This matching is safe assuming parsing is correct *)
      let [e] = expr_list in
      let (te, ty) as ty_e = type_expr e in
      if ty <> Tbool then
        raise (Error (e.pexpr_loc, Type_mismatch ([Tbool], ty)));
      (TE_op (Op_not, [ty_e]), Tbool)
  | _ ->
    begin
      (* This matching is safe assuming parsing is correct *)
      let [e1;e2] = expr_list in
      (* First, we check the first operand's type. *)
      let (te1, ty1) as ty_e1 = type_expr e1 in
      (* List of admissible types for both operands. Empty list means any type.
       * *)
      let expected_ty1 = match o with
      | Op_eq | Op_neq -> []
      | Op_lt | Op_le | Op_gt | Op_ge | Op_add | Op_sub | Op_mul | Op_div ->
          [Tint;Treal]
      | Op_mod -> [Tint]
      | Op_and | Op_or | Op_impl -> [Tbool]
      | _ -> (* handled above *) assert false
      in
      begin match expected_ty1 with
      | [] -> ()
      | _ ->
          if not (List.mem ty1 expected_ty1) then
            raise (Error (e1.pexpr_loc, Type_mismatch (expected_ty1, ty1)));
      end;
      (* Now checking the equality of type between the operands. *)
      let (te2, ty2) as ty_e2 = type_expr e2 in
      if ty1 <> ty2 then
        raise (Error (e2.pexpr_loc, Type_mismatch ([ty1], ty2)));
      (TE_op (o, [te1;te2]), ty1)
    end
