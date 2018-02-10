(* The scheduling consists in the topological sort of a graph of variables.
   * the nodes of the graph are variable names
   * an edge goes from variable A to variable B iff A's defining equation has a
     direct dependency to B.
 *)

open Typed_ast

exception Scheduling

module S = Set.Make(String)

(* The graph is represented by a mapping from variable names to sets of
 * variable names. The set associated to the variable name [id] is the set of
 * direct dependencies of the equation where [id] is defined. If an equation
 * defines several variables, their will be a duplication of information in the
 * graph. For simplicity's sake, we did not try to optimize further. *)
module M = Map.Make(String)

let rec direct_deps expr = match expr.texpr_desc with
| TE_const c -> S.empty
| TE_ident id -> S.singleton id
| TE_op (_, expr_list) | TE_app (_, expr_list) | TE_tuple expr_list ->
    List.fold_left
      (fun set expr ->
        S.union (direct_deps expr) set
      )
      S.empty
      expr_list
| TE_arrow (e1,e2) ->
    S.union (direct_deps e1) (direct_deps e2)
| TE_fby (_,_) ->
    S.empty
| TE_when (e, (id,_)) ->
    S.add id (direct_deps e)
| TE_merge ((id,_), ift, iff) ->
    S.add id @@ S.union (direct_deps ift) (direct_deps iff)

let topological_sort var_eq_map graph =
  let rec aux acc graph =
    if M.is_empty graph then acc
    else begin
      (* Take apart the minimal elements (that have no dependencies) *)
      let min_elems,rest = M.partition (fun _ deps -> S.is_empty deps) graph in
      if M.is_empty min_elems then
        raise Scheduling;
      let min_elems_s = S.of_list @@ List.map fst @@ M.bindings min_elems in
      (* Remove dependencies to [min_elems] in [rest] *)
      let rest = M.map (fun deps -> S.diff deps min_elems_s) rest in
      (* Add the minimal equations to the list, without duplicates *)
      let acc' = List.fold_left
        (fun acc var_id ->
          let eq = M.find var_id var_eq_map in
          if not (List.mem eq acc) then
             eq :: acc
          else acc
        )
        acc
        (S.elements min_elems_s)
      in
      aux acc' rest
    end
  in
  List.rev @@ aux [] graph

let schedule inputs equs =
  let input_set = S.of_list inputs in
  (* Construct a map associating every variable with its defining equation. *)
  let var_eq_map = List.fold_left
    (fun map eq ->
      let defined = Typing.defined_idents eq in
      List.fold_left
        (fun map id -> M.add id eq map)
        map
        defined
    )
    M.empty
    equs
  in
  (* Construct the graph of dependencies (see comment at head of file). *)
  let graph = List.fold_left
    (fun graph eq ->
      let defined = Typing.defined_idents eq in
      let deps = direct_deps eq.teq_expr in
      (* Remove inputs from the dependencies *)
      let deps = S.diff deps input_set in
      (* For every defined variable, add a mapping to [deps] *)
      List.fold_left
        (fun graph var_id -> M.add var_id deps graph)
        graph
        defined
    )
    M.empty
    equs
  in
  topological_sort var_eq_map graph
