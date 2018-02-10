type ident =
  { text : string;
    stamp : int }

val fresh_id : string -> ident
val fmt_ident : Format.formatter -> ident -> unit

module IdentOrd : sig
  type t = ident
  val compare : t -> t -> int
end
