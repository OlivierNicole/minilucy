open Ident
open Machine
open Clock

(* Transform clock into control structure *)
let rec control ck code =
  (* Instantiating remaining [Ck_var]s. *)
  let ck = Clocking.prune ck in
  match ck with
  | Ck_base -> code
  | Ck_on (ck, b, ck_id) ->
      if b then
        control ck MI_case (ck_id, code, MI_skip)
      else
        control ck MI_case (ck_id, MI_skip, code)
  | CK_var _ ->
      assert false (* Should not happen if clocking is correct *)

let rec join instr1 instr2 =
  match instr1,instr2 with
  | MI_case (id1, ift1, iff1), MI_case (id2, ift2, iff2) when id1 = id2 ->
      MI_case (id1, join ift1 ift2, join ift1 ift2)
  | _ ->
      MI_sequence (instr1,instr2)

let rec transl_expr ((m,si,j,d,s) as acc) expr =
  match expr.texpr_desc with
  | TE_const c -> ME_const c
  | TE_ident id ->
      if Env.mem id d then
        ME_local id
      else (* then [id] must be a state variable *)
        ME_state_var id
  | TE_op (o, expr_list) ->
      let mexpr_list = List.map (transl_expr acc) expr_list in
      ME_op (o, mexpr_list)
  | TE_when (e,_,_) ->
      transl_expr0 acc e
  | TE_tuple expr_list ->
      ME_tuple (List.map (transl_expr acc) expr_list)
  | _ ->
      assert false
      (* Other forms are managed by [instr_of_expr] and [transl_eq]. *)

let rec instr_of_expr ((m,si,j,d,s) as acc) id expr =
  match expr.texpr_desc with
  | TE_merge ((x,_), ift, iff) ->
      MI_case (x, instr_of_expr acc id ift, instr_of_expr acc id iff)
  | _ ->
      MI_assign_local (id, transl_expr acc expr)

let rec transl_eq ((m,si,j,d,s) as acc) = function
  (* x = v fby e *)
  | { teq_patt = {tpatt_idents = [x]};
      teq_expr = {texpr_desc = TE_fby (v, e) } as expr } ->
        let c = transl_expr acc e in
        let x_ty = List.head texpr.texpr_type in
        Env.add x x_ty m,
        MI_assign_state (x, ME_const v) :: si,
        j,
        d,
      control expr.texpr_clock (MI_assign_state (x, c)) :: s
  | { teq_patt = {tpatt_idents = p};
      teq_expr = {texpr_desc = TE_app (node_name, expr_list)} as expr } ->
        let mexpr_list = List.map (transl_expr acc) expr_list in
        let node_id = Ident.fresh "o" in
        m,
        MI_reset node_id :: si,
        Env.add node_id node_name j,
        d,
        control expr.texpr_clock (ME_step (p, node_id, mexpr_list)) :: s
