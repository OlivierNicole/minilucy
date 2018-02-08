module S = Set.Make(String)

let rec left expr = match expr.texpr_desc with
| TE_const c -> S.empty
| TE_ident id -> S.singleton id
| TE_op (_, expr_list) | TE_app (_, expr_list) | TE_tuple expr_list ->
    List.fold_left
      (fun set expr ->
        S.union (left expr) set
      )
      S.empty
      expr_list
| TE_arrow (e1,e2) ->
    S.union (left e1) (left e2)
| TE_fby (_,_) ->
    S.empty
| TE_when (e, (id,_)) ->
    S.add id (left e)
| TE_merge ((id,_), ift, iff) ->
    S.add id @@ S.union (left ift) (left iff)
