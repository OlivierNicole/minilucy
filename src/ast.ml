(* Arbres de syntaxe abstraite *)

open Asttypes

type ident = string

type p_expr =
  { pexpr_desc: p_expr_desc;
    pexpr_loc: location; }

and p_expr_desc =
  | PE_const of const
  | PE_ident of ident
  | PE_op of op * p_expr list
  | PE_app of ident * p_expr list
  | PE_arrow of p_expr * p_expr
  | PE_fby of const * p_expr
  | PE_tuple of p_expr list
  | PE_when of p_expr * bool * (ident * location)
  | PE_merge of (ident * location) * p_expr * p_expr
  | PE_if of (ident * location) * p_expr * p_expr
      (* if/then/else (will be desugared to [when] and [merge] during typing) *)

type p_patt =
  { ppatt_idents: string list;
    ppatt_loc: location; }

type p_equation =
    { peq_patt: p_patt;
      peq_expr: p_expr; }

type p_node =
    { pn_name: ident;
      pn_input: (ident * base_ty) list;
      pn_output: (ident * base_ty) list;
      pn_local: (ident * base_ty) list;
      pn_equs: p_equation list;
      pn_loc: location; }

type p_file = p_node list
