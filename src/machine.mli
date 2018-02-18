open Asttypes
open Ident

type m_expr =
  | ME_local of ident
  | ME_const of const
  | ME_state_var of ident
  | ME_op of op * m_expr list

type m_instr =
  | MI_assign_local of ident * m_expr
  | MI_assign_state of ident * m_expr (* Assign a state variable. *)
  | MI_sequence of m_instr * m_instr
  | MI_skip (* Does nothing *)
  | MI_reset of ident (* Reinitialization of an object *)
  | MI_step of ident list * ident * m_expr list
      (* (x_1,x_2,...,x_n) = o.step(e_1,e_2,...,e_m) *)
  | MI_case of ident * m_instr * m_instr
      (* case(x) (true -> s_1) (false -> s_2) *)

type m_step_desc =
  { ms_input : (ident * base_ty) list;
    ms_output : (ident * base_ty) list;
    ms_local : (ident * base_ty) list;
    ms_code : m_instr }

type machine =
  { m_name : string;
    m_mem : (ident * base_ty) list;
    m_instances : (ident * string) list; (* instance name, node name *)
    m_reset : m_instr;
    m_step : m_step_desc }
