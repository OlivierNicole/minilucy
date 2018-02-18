open Asttypes
open Typed_ast
open Clock

type error =
  | Clock_mismatch of clock * clock
      (* expected clock, actual clock *)

exception Error of location * error

val clock_file : t_file -> unit

val prune : clock -> clock
