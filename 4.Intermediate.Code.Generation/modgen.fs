open System
open System.Collections
open System.Collections.Generic
open System.IO

let rnd = new Random()
let getRandom (n : int) = rnd.Next(n)

type name = string
and expr = Expr0 of simplexpr | Expr1 of simplexpr * relOp * simplexpr
and simplexpr = Simplexpr0 of term | Simplexpr1 of posneg * term | Simplexpr2 of simplexpr * termOp * term
and term = Term0 of factor | Term1 of term * factOp * factor
and factor = Factor0 of qualident | Factor1 of int | Factor2 of bool | Factor3 of char | Factor4 of string
           | Factor5 of expr | Factor6 of call | Factor7 of bang * factor
and qualident = Qualident0 of name | Qualident1 of name * int list
and relOp = Eq | Neq | Le | Lt | Ge | Gt
and termOp = Plus | Minus | Or
and factOp = Mult | Div | And
and posneg = Pos | Neg
and bang = Bang
and call = Call0 of name * expr list

type symtab = string (*TODO*)

let get_int (st:symtab) : qualident = Qualident0 "TODO"
let get_bool (st:symtab) : qualident = Qualident0 "TODO"

let rec make_int_expr (depth:int) (st:symtab) : expr = Expr0 (make_int_simplexpr (depth-1) st)
and make_int_simplexpr (depth:int) (st:symtab) : simplexpr = 
  match (getRandom 3) with
  | 0 -> Simplexpr0 (make_int_term (depth-1) st)
  | 1 -> let op = (match (getRandom 2) with 0 -> Pos | _ -> Neg) in
          Simplexpr1 (op, (make_int_term (depth-1) st))
  | _ -> let op = (match (getRandom 2) with 0 -> Plus | _ -> Minus) in
          Simplexpr2 ((make_int_simplexpr (depth-1) st), op, (make_int_term (depth-1) st))
and make_int_term (depth:int) (st:symtab) : term =
  match (getRandom 2) with
  | 0 -> Term0 (make_int_factor (depth-1) st)
  | _ -> let op = (match (getRandom 2) with 0 -> Mult | _ -> Div) in
         Term1 ((make_int_term (depth-1) st), op, (make_int_factor (depth-1) st))
and make_int_factor (depth:int) (st:symtab) : factor =
  match (getRandom 4) with
  | 0 -> Factor0 (get_int st)
  | 1 -> Factor1 (getRandom 1000000)
  | 2 -> Factor5 (make_int_expr (depth-1) st)
  | _ -> Factor6 (make_int_call (depth-1) st)
and make_int_call (depth:int) (st:symtab) : call =
  Call0 ("TODO", [])

and make_bool_expr (depth:int) (st:symtab) : expr =
  match (getRandom 3) with
  | 0 -> Expr0 (make_bool_simplexpr (depth-1) st)
  | 1 -> let op = (match (getRandom 2) with 0 -> Eq | _ -> Neq) in
         let f = (match (getRandom 2) with 0 -> make_int_simplexpr | 1 -> make_bool_simplexpr | _ -> make_char_simplexpr) in
         Expr1 ((f (depth-1) st), op, (f (depth-1) st))
  | _ -> let op = (match (getRandom 4) with 0 -> Lt | 1 -> Le | 2 -> Gt | _ -> Ge) in
         let f = (match (getRandom 2) with 0 -> make_int_simplexpr | _ -> make_char_simplexpr) in
         Expr1 ((f (depth-1) st), op, (f (depth-1) st))
and make_bool_simplexpr (depth:int) (st:symtab) : simplexpr =
  match (getRandom 2) with
  | 0 -> Simplexpr0 (make_bool_term (depth-1) st)
  | _ -> Simplexpr2 ((make_bool_simplexpr (depth-1) st), Or, (make_bool_term (depth-1) st))
and make_bool_term (depth:int) (st:symtab) : term =
  match (getRandom 2) with
  | 0 -> Term0 (make_bool_factor (depth-1) st)
  | _ -> Term1 ((make_bool_term (depth-1) st), And, (make_bool_factor (depth-1) st))
and make_bool_factor (depth:int) (st:symtab) : factor =
  match (getRandom 5) with
  | 0 -> Factor0 (get_bool st)
  | 1 -> let v = (match (getRandom 2) with 0 -> true | _ -> false) in
         Factor2 v
  | 2 -> Factor5 (make_bool_expr (depth-1) st)
  | 3 -> Factor6 (make_bool_call (depth-1) st)
  | _ -> Factor7 (Bang, (make_bool_factor (depth-1) st))
and make_bool_call (depth:int) (st:symtab) : call =
  Call0 ("TODO", [])

and make_char_expr (depth:int) (st:symtab) : expr = Expr0 (make_char_simplexpr (depth-1) st)
and make_char_simplexpr (depth:int) (st:symtab) : simplexpr = Simplexpr0 (make_char_term (depth-1) st)
and make_char_term (depth:int) (st:symtab) : term = Term0 (make_char_factor (depth-1) st)
and make_char_factor (depth:int) (st:symtab) : factor = 
  match (getRandom 2) with
  | 0 -> Factor3 'h'
  | 1 -> Factor5 (make_char_expr (depth-1) st)
  | _ -> Factor6 (make_char_call (depth-1) st)
and make_char_call (depth:int) (st:symtab) : call =
  Call0 ("TODO", [])