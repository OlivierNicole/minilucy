open Asttypes
open Typed_ast
open Clock
open Ident

type error =
  | Clock_mismatch of clock * clock
      (* expected clock, actual clock *)

exception Error of location * error

module Env = Map.Make(IdentOrd)

let ckvar_counter = ref 0

let fresh_ck =
  let ckvar_counter = ref 0 in
  fun () ->
    Ck_var {
      ckv_id = (incr ckvar_counter; !ckvar_counter);
      ckv_inst = None }

let rec prune = function
  | Ck_var ({ ckv_inst = Some inst } as v) ->
      let ninst = prune inst in
      v.ckv_inst <- Some ninst;
      ninst
  | c -> c

let rec unify loc c1 c2 =
  let a = prune c1 in
  let b = prune c2 in
  match a,b with
  | Ck_var v, other | other, Ck_var v ->
    begin match other with
    | Ck_var v' when v.ckv_id = v'.ckv_id -> ()
    | _ -> v.ckv_inst <- Some other
    end
  | Ck_on (ck_a, b_a, id_a), Ck_on (ck_b, b_b, id_b) ->
      unify loc ck_a ck_b;
      if id_a <> id_b || b_a <> b_b then
        raise (Error (loc, Clock_mismatch (a, b)))
  | Ck_base, Ck_base -> ()
  | _ ->
      raise (Error (loc, Clock_mismatch (a, b)))

let unify_list loc l1 l2 =
  assert (List.length l1 = List.length l2);
  List.iter2 (unify loc) l1 l2

let rec repeat n x =
  if n <= 0 then []
  else x :: repeat (pred n) x

let rec clock_expr loc env expr =
  match expr.texpr_desc with
  | TE_const _ ->
      expr.texpr_clock <- [fresh_ck ()]
  | TE_ident id ->
      let ck = Env.find id env in
      expr.texpr_clock <- [ck]
  | TE_op (o, expr_list) ->
      let ck = fresh_ck () in
      List.iter
        (fun expr ->
          clock_expr loc env expr;
          unify loc ck (List.hd expr.texpr_clock);
        )
        expr_list;
      expr.texpr_clock <- [ck]
  | TE_app (f_id, expr_list) ->
      let nb_outputs = List.length expr.texpr_type in
      let ck = fresh_ck () in
      List.iter
        (fun expr ->
          clock_expr loc env expr;
          let nb_elts = List.length expr.texpr_clock in
          unify_list loc (repeat nb_elts ck) expr.texpr_clock;
        )
        expr_list;
      expr.texpr_clock <- repeat nb_outputs ck
  | TE_arrow (e1,e2) ->
      clock_expr loc env e1;
      clock_expr loc env e2;
      unify_list loc e1.texpr_clock e2.texpr_clock;
      expr.texpr_clock <- e1.texpr_clock
  | TE_fby (_, e) ->
      clock_expr loc env e;
      expr.texpr_clock <- e.texpr_clock
  | TE_tuple expr_list ->
      List.iter (clock_expr loc env) expr_list;
      expr.texpr_clock <- List.concat @@
        List.map (fun e -> e.texpr_clock) expr_list
  | TE_when (e, b, (id,_)) ->
      let ck = Env.find id env in
      clock_expr loc env e;
      let nb_elts = List.length e.texpr_clock in
      unify_list loc (repeat nb_elts ck) e.texpr_clock;
      expr.texpr_clock <- repeat nb_elts (Ck_on (ck, b, id))
  | TE_merge ((id,_), ift, iff) ->
      let ck = Env.find id env in
      clock_expr loc env ift;
      clock_expr loc env iff;
      let nb_outputs = List.length ift.texpr_clock in
      unify_list loc ift.texpr_clock @@
        repeat nb_outputs (Ck_on (ck, true, id));
      unify_list loc iff.texpr_clock @@
        repeat nb_outputs (Ck_on (ck, false, id));
      expr.texpr_clock <- repeat nb_outputs ck

let clock_equation env { teq_patt = pat; teq_expr = expr } =
  clock_expr expr.texpr_loc env expr;
  let defined_ids = pat.tpatt_idents in
  let expected = expr.texpr_clock in
  let actual = List.map (fun id -> Env.find id env) defined_ids in
  unify_list pat.tpatt_loc actual expected

let clock_node node =
  (* Set all input and output clocks to the base clock *)
  let env = List.fold_left
    (fun env id -> Env.add id Ck_base env)
    Env.empty
    (List.map fst (node.tn_input @ node.tn_output))
  in
  (* Insert fresh clock variables for local identifiers *)
  let env = List.fold_left
    (fun env id -> Env.add id (fresh_ck ()) env)
    env
    (List.map fst node.tn_local)
  in
  List.iter (clock_equation env) node.tn_equs

let clock_file nodes =
  List.iter clock_node nodes

let report_error fmt = function
  | Clock_mismatch (ck_expect, ck_actual) ->
      Format.fprintf fmt "Expected clock %a, but got %a."
        Clock.fmt_clock ck_expect Clock.fmt_clock ck_actual
