open Typed_ast
open Ident

let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)

let mkpat ids =
  { tpatt_idents = ids;
    tpatt_loc = dummy_loc }

type acc =
  { ac_new_eqs : t_equation list;
    ac_env : ty Env.t }



let rec normalize_expr acc expr =
  match expr.texpr_desc with
  | TE_const _ | TE_ident _ -> expr, acc
  | TE_op (o, expr_list) ->
      let exprs,acc = List.split @@ List.fold_right
        (fun expr (exprs,acc) ->
          let expr,acc = normalize_expr acc expr in
          expr :: exprs, acc
        )
        expr_list
        ([],acc)
      in
      let new_id = fresh_id "op" in
      let new_eq =
        { teq_patt = mkpat [new_id];
          teq_expr = { expr with texpr_desc = TE_op (o, exprs) } }
      in
      { expr with texpr_desc = TE_ident new_id },
      { ac_new_eqs = new_eq :: acc.ac_new_eqs;
        ac_env = Env.add new_id (List.hd expr.texpr_type) }
