open Asttypes
open Typed_ast
open Ident

let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)

let mkpat ids =
  { tpatt_idents = ids;
    tpatt_loc = dummy_loc }

module Env = Map.Make(IdentOrd)

type acc =
  { ac_new_eqs : t_equation list;
    ac_env : base_ty Env.t }

let rec repeat n x =
  if n <= 0 then []
  else x :: repeat (pred n) x

(* [toplevel] is a boolean stating whether the expression is at top level in
 * its equation. This avoids unnecessary generation of equations of the form
 *   x = y
 *)
let rec normalize_expr acc toplevel expr =
  match expr.texpr_desc with
  | TE_const _ | TE_ident _ -> expr, acc
  | TE_op (o, expr_list) ->
      let exprs,acc = norm_list acc expr_list in
      if toplevel then
        { expr with texpr_desc = TE_op (o, exprs) },
        acc
      else
        let new_id = fresh_id "op" in
        let new_eq =
          { teq_patt = mkpat [new_id];
            teq_expr = { expr with texpr_desc = TE_op (o, exprs) } }
        in
        { expr with texpr_desc = TE_ident new_id },
        { ac_new_eqs = new_eq :: acc.ac_new_eqs;
          ac_env = Env.add new_id (List.hd expr.texpr_type) acc.ac_env }
  | TE_app (node_id, expr_list) ->
      let exprs,acc = norm_list acc expr_list in
      if toplevel then
        { expr with texpr_desc = TE_app (node_id, exprs) },
        acc
      else
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
                  texpr_clock = [ck];
                  texpr_loc = dummy_loc }
              )
              (List.combine new_ids expr.texpr_type)
              expr.texpr_clock
            );
        },
        { ac_new_eqs = new_eq :: acc.ac_new_eqs;
          ac_env = List.fold_left2
            (fun env id base_ty -> Env.add id base_ty env)
            acc.ac_env
            new_ids
            expr.texpr_type
        }
  | TE_arrow (e1,e2) ->
      let e2',acc = normalize_expr acc false e2 in
      let e1',acc = normalize_expr acc false e1 in
      if toplevel then
        { expr with texpr_desc = TE_arrow (e1',e2') },
        acc
      else
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
                  texpr_clock = [ck];
                  texpr_loc = dummy_loc }
              )
              (List.combine new_ids expr.texpr_type)
              expr.texpr_clock
            );
        },
        { ac_new_eqs = new_eq :: acc.ac_new_eqs;
          ac_env = List.fold_left2
            (fun env id base_ty -> Env.add id base_ty env)
            acc.ac_env
            new_ids
            expr.texpr_type; }
  | TE_fby (c,e) ->
      let e',acc = normalize_expr acc false e in
      if toplevel then
        { expr with texpr_desc = TE_fby (c,e') },
        acc
      else
        let nb_outputs = List.length expr.texpr_type in
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
            (fun env id base_ty -> Env.add id base_ty env)
            acc.ac_env
            new_ids
            expr.texpr_type; }
  | TE_tuple expr_list ->
      let exprs,acc = norm_list acc expr_list in
      { expr with texpr_desc = TE_tuple exprs },
      acc
  | TE_when (e, b, localized_id) ->
      let e',acc = normalize_expr acc false e in
      { expr with texpr_desc = TE_when (e', b, localized_id) },
      acc
  | TE_merge (localized_id, ift, iff) ->
      let iff',acc = normalize_expr acc false iff in
      let ift',acc = normalize_expr acc false ift in
      let nb_outputs = List.length expr.texpr_type in
      if toplevel then
        { expr with texpr_desc = TE_merge (localized_id, ift', iff') },
        acc
      else if nb_outputs = 1 then
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
            (fun env id base_ty -> Env.add id base_ty env)
            acc.ac_env
            new_ids
            expr.texpr_type; }
      else


(* Normalize a list of expressions. New expressions and bindings are
 * accumulated starting from the right. *)
and norm_list acc expr_list =
  List.fold_right
    (fun expr (exprs,acc) ->
      let expr,acc = normalize_expr acc false expr in
      expr :: exprs, acc
    )
    expr_list
    ([],acc)

let rec normalize_equs eqs =
  let rec aux acc = function
  | [] -> acc
  | eq :: eqs ->
    begin match eq with
    | { teq_patt = { tpatt_idents = ids };
        teq_expr = {texpr_desc = TE_tuple expr_list} } ->
          (* Add one equation per identifier *)
          let acc = List.fold_left2
            (fun acc expr id ->
              let expr',acc = normalize_expr acc true expr in
              { acc with ac_new_eqs =
                { teq_patt =
                  { tpatt_idents = [id]; tpatt_loc = dummy_loc };
                  teq_expr = expr'
              } :: acc.ac_new_eqs }
            )
            acc
            expr_list
            ids
          in
          aux acc eqs
    | _ ->
          let expr',acc = normalize_expr acc true eq.teq_expr in
          aux
            { acc with ac_new_eqs =
              { eq with teq_expr = expr' } :: acc.ac_new_eqs }
            eqs
    end
  in
  let acc = aux { ac_new_eqs = []; ac_env = Env.empty } eqs in
  List.rev acc.ac_new_eqs, acc.ac_env

let normalize_node node =
  let equs,new_env = normalize_equs node.tn_equs in
  let local = node.tn_local @ Env.bindings new_env in
  { node with
      tn_equs = equs;
      tn_local = local }

let normalize_file nodes =
  List.map normalize_node nodes
