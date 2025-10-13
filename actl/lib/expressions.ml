(* Core ATL Expression Types - Clean and focused *)

type pat =
  | PVar of string          (* Variable pattern *)
  | PAtom of string         (* Atom pattern *)  
  | PList of pat list       (* Tuple/List pattern *)
  | PWildcard               (* Wildcard pattern _ *)

type binop = 
  | Add | Sub | Mult | Div  (* Arithmetic *)
  | Eq | Neq | Lt | Gt | Lte | Gte  (* Comparison *)
  | AndOp | OrOp            (* Logical - avoiding OCaml reserved words *)

type constraint_value = 
  | CTrue                   (* ⊤ - always true *)
  | CFalse                  (* ⊥ - always false *)
  | CBool of bool           (* Boolean literal *)
  | CVar of string          (* Variable in constraint *)
  | CNum of int             (* Numeric literal *)
  | CBinOp of binop * constraint_value * constraint_value  (* Binary operations *)
  | CNot of constraint_value     (* Negation *)

(* Message signature: send_{src→dst}{M} *)
type message_sig = 
  | Send of string * string * pat

(* Core WALTZ Formula - Clean and focused *)
type atl_formula =
  | Signature of message_sig * constraint_value      (* α : δ - Basic message constraint *)
  | Disjunction of atl_formula * atl_formula    (* φ ∨ φ - Either formula *)
  | Conjunction of atl_formula * atl_formula    (* φ ∧ φ - Both formulas *)
  | Sequence of atl_formula * atl_formula       (* φ ; φ - Chaining *)
  | Omega of atl_formula                        (* Ω(φ) - Omega *)
  | Theta of atl_formula                        (* Θ(φ) - Theta *)
  | Top                                         (* ⊤ - Always true *)
  | Bottom                                      (* ⊥ - Always false *)

(* Utility functions for pattern handling *)
let rec pat_to_string = function
  | PAtom s -> s
  | PVar s -> s
  | PWildcard -> "_"
  | PList ps -> "{" ^ String.concat ", " (List.map pat_to_string ps) ^ "}"

let rec extract_variables_from_pattern = function
  | PVar v -> [v]
  | PAtom _ -> []
  | PWildcard -> []
  | PList ps -> List.flatten (List.map extract_variables_from_pattern ps)