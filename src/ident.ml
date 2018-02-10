type ident =
  { text : string;
    stamp : int }

let fresh_id =
  let counter = ref 0 in
  fun name ->
    { text = name;
      stamp = (incr counter; !counter) }

let fmt_ident fmt { text = t; stamp = i } =
  Format.fprintf fmt "%s_%i" t i

module IdentOrd = struct
  type t = ident
  let compare = Pervasives.compare
end
