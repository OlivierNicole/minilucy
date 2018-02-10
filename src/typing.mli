open Asttypes
open Ast
module T = Typed_ast
open T
open Ident

type error =
  | Unbound_ident of string
  | Type_mismatch of ty list * ty
      (* list of possible types, actual type *)
  | Undeclared_ident of string
      (* When an equation defines a identifier that is not declared as a local
       * variable *)
  | Duplicate_node_decl of string
      (* When there are two node declarations with the same name *)
  | Duplicate_local_decl of string
      (* When a local identifier is declared more than once in a node. *)
  | Undefined_local of ident

exception Error of location * error

type env
type node_env

val type_file : p_file -> t_file

val report_error : Format.formatter -> error -> unit
