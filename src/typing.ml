open Ast
module T = Typed_ast
open T

type error =
  | Unbound_ident of T.ident
  | Type_mismatch of base_ty * base_ty (* expected type, actual type *)

exception Error of error

let type_const = function
  | Cbool -> Tbool
  | Cint -> Tint
  | Creal -> Treal

let rec type_expr env { pexpr_desc = pdesc } =
  match pdesc with
  | PE_const c ->
      (TE_const c, type_const c)
  | PE_ident id ->
    begin try
      let ty = Env.find id env in
      (TE_ident id, ty)
    with Not_found ->
      raise (Error (Unbound_ident id))
    end
  | PE_op (o, expr_list) ->
      type_op o e1 e2

and type_op o expr_list =
  (* List of expected types for the operands, in the form of a [base_ty list
   * list]. Each sublist is a list of possible types for the operand. Empty
   * list means any type. *)
  let expected_type_list = match o with
  | Op_eq | Op_neq -> [[]; []]
  | Op_lt | Op_le | Op_gt | Op_ge | Op_add | Op_sub | Op_mul | Op_div ->
      [[Tint;Treal]; [Tint;Treal]]
  | Op_mod ->
      [[Tint];[Tint]]
  | Op_not -> [[Tbool]]
  | Op_and | Op_or | Op_impl ->
      [[Tbool];[Tbool]]
  | Op_if ->
      [[Tbool];[];[]]
  in
  let type_list = List.map type_expr expr_list in
  let check_inclusion ty expect_tys =
    if not (List.mem ty expect_tys) then
      raise Error (Type_mismatch 
