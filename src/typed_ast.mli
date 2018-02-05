(* Arbre de syntaxe abstraite typé *)

open Asttypes

type ident = string

module Env = Map.Make(String)

type env = base_ty Env.t

type t_expr =
  { texpr_desc: t_expr_desc;
    texpr_type: base_ty;
    texpr_loc: location; }

and t_expr_desc =
  | TE_const of const
  | TE_ident of ident
  | TE_op of op * t_expr list
  | TE_app of ident * t_expr list
  | TE_arrow of t_expr * t_expr
  | TE_pre of t_expr
  | TE_tuple of t_expr list

type t_patt =
  { tpatt_desc: t_patt_desc;
    tpatt_loc: location; }

and t_patt_desc =
  | PP_ident of ident
  | PP_tuple of ident list

type t_equation =
    { teq_patt: t_patt;
      teq_expr: t_expr; }

type t_node =
    { tn_name: ident;
      tn_input: (ident * base_ty) list;
      tn_output: (ident * base_ty) list;
      tn_local: (ident * base_ty) list;
      tn_equs: t_equation list;
      tn_loc: location; }

type t_file = t_node list
