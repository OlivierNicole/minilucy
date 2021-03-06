open Ident
open Typed_ast
open Machine
open Clock

module Env = Map.Make(IdentOrd)

(* Transform clock into control structure *)
let rec control ck code =
  (* Instantiating remaining [Ck_var]s. *)
  let ck = Clocking.prune ck in
  match ck with
  | Ck_base -> code
  | Ck_on (ck, b, ck_id) ->
      if b then
        control ck (MI_case (ck_id, code, MI_skip))
      else
        control ck (MI_case (ck_id, MI_skip, code))
  | Ck_var _ ->
      (* An uninstantiated clock after clocking means that the expression
       * should have the base clock. *)
      code

let rec join instr1 instr2 =
  match instr1,instr2 with
  | MI_case (id1, ift1, iff1), MI_case (id2, ift2, iff2) when id1 = id2 ->
      MI_case (id1, join ift1 ift2, join ift1 ift2)
  | _ ->
      MI_sequence (instr1,instr2)

let rec join_list = function
  | [] -> MI_skip
  | [i] -> i
  | i :: is -> join i (join_list is)

let rec transl_expr ((m,si,j,d,s) as acc) expr =
  match expr.texpr_desc with
  | TE_const c -> ME_const c
  | TE_ident id ->
      if Env.mem id d then
        ME_local id
      else if Env.mem id m then
        ME_state_var id
      else begin
        Format.printf "%a\n%!" fmt_ident id;
        assert false
      end
  | TE_op (o, expr_list) ->
      let mexpr_list = List.map (transl_expr acc) expr_list in
      ME_op (o, mexpr_list)
  | TE_when (e,_,_) ->
      transl_expr acc e
  | TE_tuple [e] ->
      transl_expr acc e
  | TE_tuple _ ->
      (* There should not be "real" tuples after normalization. *)
      assert false
  | _ ->
      Format.printf "%a\n%!" Tast_printer.expr expr;
      assert false
      (* Other forms are managed by [instr_of_expr] and [transl_eq]. *)

let rec instr_of_expr ((m,si,j,d,s) as acc) id expr =
  match expr.texpr_desc with
  | TE_merge ((x,_), ift, iff) ->
      MI_case (x, instr_of_expr acc id ift, instr_of_expr acc id iff)
  | _ ->
      if Env.mem id d then
        MI_assign_local (id, transl_expr acc expr)
      else if Env.mem id m then
        MI_assign_state (id, transl_expr acc expr)
      else assert false

let memory_vars_eq acc = function
  | { teq_patt = {tpatt_idents = [x]};
      teq_expr = {texpr_desc = TE_fby (v, e) } as expr } ->
        let x_ty = List.hd expr.texpr_type in
        Env.add x x_ty acc
  | _ ->
      acc

let rec memory_vars acc eqs =
  List.fold_left memory_vars_eq acc eqs

let transl_eq ((m,si,j,d,s) as acc) = function
  (* x = v fby e *)
  | { teq_patt = {tpatt_idents = [x]};
      teq_expr = {texpr_desc = TE_fby (v, e) } as expr } ->
        let c = transl_expr acc e in
        m,
        MI_assign_state (x, ME_const v) :: si,
        j,
        d,
        control (List.hd expr.texpr_clock) (MI_assign_state (x, c)) :: s
  | { teq_patt = {tpatt_idents = p};
      teq_expr = {texpr_desc = TE_app (node_name, expr_list)} as expr } ->
        let mexpr_list = List.map (transl_expr acc) expr_list in
        let node_id = fresh_id "o" in
        m,
        MI_reset node_id :: si,
        Env.add node_id node_name j,
        d,
        control (List.hd expr.texpr_clock) (MI_step (p, node_id, mexpr_list))
          :: s
  | { teq_patt = {tpatt_idents = [x]};
      teq_expr = expr } ->
        m,
        si,
        j,
        d,
        control (List.hd expr.texpr_clock) (instr_of_expr acc x expr) :: s
  | _ ->
      (* Other patterns should not exist after normalization. *)
      assert false

let rec transl_eqs acc = function
  | [] -> acc
  | eq :: eqs ->
      transl_eq (transl_eqs acc eqs) eq

let env_of_list (l : (ident * 'a) list) =
  List.fold_left
    (fun env (id,x) ->
      Env.add id x env
    )
    Env.empty
    l

let transl_node node =
  let mem = memory_vars (env_of_list node.tn_output) node.tn_equs in
  (* The local variables (stack variables) of the step function are the local
   * variables of the node, minus those that are in the machine memory. *)
  let locals = Env.filter
    (fun id _ -> not (Env.mem id mem))
    (env_of_list node.tn_local)
  in
  let locals_and_inputs =
    Env.union (fun x _ -> assert false) locals (env_of_list node.tn_input)
  in
  let m,si,j,d,s =
    transl_eqs (mem,[],Env.empty,locals_and_inputs,[]) node.tn_equs
  in
  { m_name = node.tn_name;
    m_mem = Env.bindings m;
    m_instances = Env.bindings j;
    m_reset = join_list si;
    m_step =
      { ms_input = node.tn_input;
        ms_output = node.tn_output;
        ms_local = Env.bindings locals;
        ms_code = join_list s } }

let transl_file nodes =
  List.map transl_node nodes
