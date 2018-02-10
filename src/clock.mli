open Ident

type ckvar =
  { ckv_id : int;
    mutable ckv_inst : clock option }

and clock =
  | Ck_base
  | Ck_on of clock * bool * ident
  | Ck_var of ckvar

val fmt_clock : Format.formatter -> clock -> unit
