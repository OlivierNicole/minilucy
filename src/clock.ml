open Ident

type ckvar =
  { ckv_id : int;
    mutable ckv_inst : clock option }

and clock =
  | Ck_base
  | Ck_on of clock * bool * ident
  | Ck_var of ckvar

open Format

let rec fmt_clock fmt = function
  | Ck_base -> fprintf fmt "base"
  | Ck_on (ck,b,id) ->
      fprintf fmt "(%a) on %b(%a)" fmt_clock ck b fmt_ident id
  | Ck_var { ckv_id = id; ckv_inst = Some ck } ->
      fprintf fmt "Ck_var (%i, (Some %a))" id fmt_clock ck
  | Ck_var { ckv_id = id; ckv_inst = None } ->
      fprintf fmt "Ck_var (%i, None)" id
