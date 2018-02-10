(* Arbre de syntaxe abstraite typé *)

open Asttypes
open Clock
open Ident

type t_expr =
  { texpr_desc: t_expr_desc;
    texpr_type: ty;
    mutable texpr_clock: clock list;
    texpr_loc: location; }

and t_expr_desc =
  | TE_const of const
  | TE_ident of ident
  | TE_op of op * t_expr list
  | TE_app of string * t_expr list
  | TE_arrow of t_expr * t_expr
  | TE_fby of const * t_expr
  | TE_tuple of t_expr list
  | TE_when of t_expr * bool * (ident * location)
  | TE_merge of (ident * location) * t_expr * t_expr

type t_patt =
  { tpatt_idents: ident list;
    tpatt_loc: location; }

type t_equation =
    { teq_patt: t_patt;
      teq_expr: t_expr; }

type t_node =
    { tn_name: string;
      tn_input: (ident * base_ty) list;
      tn_output: (ident * base_ty) list;
      tn_local: (ident * base_ty) list;
      tn_equs: t_equation list;
      tn_loc: location; }

type t_file = t_node list
