open Asttypes
open Ast
module T = Typed_ast
open T

type error =
  | Unbound_ident of T.ident
  | Type_mismatch of ty list * ty
      (* list of possible types, actual type *)
  | Undeclared_ident of T.ident
      (* When an equation defines a identifier that is not declared as a local
       * variable *)
  | Duplicate_node_decl of T.ident
      (* When there are two node declarations with the same name *)
  | Duplicate_local_decl of T.ident
      (* When a local identifier is declared more than once in a node. *)
  | Undefined_local of T.ident

exception Error of location * error

type env
type node_env

val type_file : p_file -> t_file

val defined_idents : t_equation -> ident list

val report_error : Format.formatter -> error -> unit
