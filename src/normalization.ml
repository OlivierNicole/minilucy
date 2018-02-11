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
      let exprs,acc = norm_list acc expr_list in
      let new_id = fresh_id "op" in
      let new_eq =
        { teq_patt = mkpat [new_id];
          teq_expr = { expr with texpr_desc = TE_op (o, exprs) } }
      in
      { expr with texpr_desc = TE_ident new_id },
      { ac_new_eqs = new_eq :: acc.ac_new_eqs;
        ac_env = Env.add new_id (List.hd expr.texpr_type) }
  | TE_app (node_id, expr_list) ->
      let exprs,acc = norm_list acc expr_list in
      let nb_outputs = List.length expr.texpr_type in
      let new_ids = List.map fresh_id @@ repeat nb_outputs "app" in
      let new_eq =
        { teq_patt = mkpat new_ids;
          teq_expr = { expr with texpr_desc = TE_app (node_id, exprs) } }
      in
      { expr with texpr_desc = TE_tuple
          (List.map2
            (fun (id,bty) ck ->
              { texpr_desc = TE_ident id;
                texpr_type = [bty];
                texpr_clock = ck;
                texpr_loc = dummy_loc }
            )
            (List.combine new_ids expr.texpr_type)
            expr.texpr_clock
          );
      },
      { ac_new_eqs = new_eq :: acc.ac_new_eqs;
        ac_env = List.fold_left2
          (fun env id base_ty -> Env.add id base.ty env)
          acc.ac_env
          new_ids
          expr.texpr_type
      }
  | TE_arrow (e1,e2) ->
      let e2',acc = normalize_expr acc e2 in
      let e1',acc = normalize_expr acc e1 in
      let nb_outputs = List.length expr.texpr_type in
      let new_ids = List.map fresh_id @@ repeat nb_outputs "arr" in
      let new_eq =
        { teq_patt = mkpat new_ids;
          teq_expr = { expr with texpr_desc = TE_arrow (e1',e2') } }
      in
      { expr with texpr_desc = TE_tuple
          (List.map2
            (fun (id,bty) ck ->
              { texpr_desc = TE_ident id;
                texpr_type = [bty];
                texpr_clock = ck;
                texpr_loc = dummy_loc }
            )
            (List.combine new_ids expr.texpr_type)
            expr.texpr_clock
          );
      },
      { ac_new_eqs = new_eq :: acc.ac_new_eqs;
        ac_env = List.fold_left2
          (fun env id base_ty -> Env.add id base.ty env)
          acc.ac_env
          new_ids
          expr.texpr_type; }
  | TE_fby (c,e) ->
      let e',acc = normalize_expr acc e in
      let nb_outputs = List.length expr.expr_type in
      let new_ids = List.map fresh_id @@ repeat nb_outputs "fb" in
      let new_eq =
        { teq_patt = mkpat new_ids;
          teq_expr = { expr with texpr_desc = TE_fby (c,e') } }
      in
      { expr with texpr_desc = TE_tuple
          (List.map2
            (fun (id,bty) ck ->
              { texpr_desc = TE_ident id;
                texpr_type = [bty];
                texpr_clock = [ck];
                texpr_loc = dummy_loc }
            )
            (List.combine new_ids expr.texpr_type)
            expr.texpr_clock
          );
      },
      { ac_new_eqs = new_eq :: acc.ac_new_eqs;
        ac_env = List.fold_left2
          (fun env id base_ty -> Env.add id base.ty env)
          acc.ac_env
          new_ids
          expr.texpr_type; }
  | TE_tuple expr_list ->
      let exprs,acc = norm_list acc expr_list in
      { expr with expr_desc = TE_tuple exprs },
      acc
  | TE_when (e, b, localized_id) ->
      let e',acc = normalize_expr in
      { expr with texpr_desc = TE_when (e', b, localized_id) },
      acc
  | TE_merge (localized_id, ift, iff) ->
      let iff' = normalize_expr acc iff in
      let ift' = normalize_expr acc ift in
      let nb_outputs = List.length expr.texpr_type in
      let new_ids = List.map fresh_id @@ repeat nb_outputs "merge" in
      let new_eq =
        { teq_patt = mkpat new_ids;
          teq_expr = { expr with texpr_desc =
            TE_merge (localized_id, ift', iff') } }
      in
      { expr with texpr_desc = TE_tuple
          (List.map2
            (fun (id,bty) ck ->
              { texpr_desc = TE_ident id;
                texpr_type = [bty];
                texpr_clock = [ck];
                texpr_loc = dummy_loc }
            )
            (List.combine new_ids expr.texpr_type)
            expr.texpr_clock
          );
      },
      { ac_new_eqs = new_eq :: acc.ac_new_eqs;
        ac_env = List.fold_left2
          (fun env id base_ty -> Env.add id base.ty env)
          acc.ac_env
          new_ids
          expr.texpr_type; }

(* Normalize a list of expressions. New expressions and bindings are
 * accumulated starting from the right. *)
and norm_list acc expr_list =
  List.split @@ List.fold_right
    (fun expr (exprs,acc) ->
      let expr,acc = normalize_expr acc expr in
      expr :: exprs, acc
    )
    expr_list
    ([],acc)
