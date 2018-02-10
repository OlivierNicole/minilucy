type ident = string

type ckvar =
  { ckv_id : int;
    mutable ckv_inst : clock option }

and clock =
  | Ck_base
  | Ck_on of clock * bool * ident
  | Ck_var of ckvar
