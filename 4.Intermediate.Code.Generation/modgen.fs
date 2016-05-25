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
and qualident = Qualident0 of name | Qualident1 of qualident * expr
and relOp = Eq | Neq | Le | Lt | Ge | Gt
and termOp = Plus | Minus | Or
and factOp = Mult | Div | And
and posneg = Pos | Neg
and bang = Bang
and call = Call0 of name * expr list
and basetyp = NULL | INT | CHAR | BOOL
and typ = Typ0 of basetyp | Typ1 of typ * int

type symtab = Dictionary<typ, List<name>>

let get_random_elem (l:List<'a>) = l.[(getRandom(l.Count))]

let rec make_array_type (b:typ) (n:int) : typ =
  if n = 0 then b else Typ1 ((make_array_type b (n-1)), (getRandom 100))

let rec make_qualident (n:name) (t:typ) (depth:int) (st:symtab) : qualident =
  match t with
  | Typ0 _ -> Qualident0 n
  | Typ1 (inner, _) -> Qualident1 (make_qualident n inner depth st, (make_int_expr depth st))

and fresh_name (st:symtab) : name =
  let valueList = new List<name>()
  Seq.iter (fun (KeyValue((k : typ), (v : List<name>))) -> v.ForEach (fun (x : name) -> valueList.Add(x))) st
  let mutable cnt = 0
  let mutable brk = false
  let mutable tempStr = ""
  while (not brk) do
    tempStr <- "t" + cnt.ToString()
    if not (valueList.Contains(tempStr)) then brk <- true
  tempStr

and new_symtab () : symtab =
  new Dictionary<typ, List<name>>()

and get_int (depth:int) (st:symtab) : qualident =
  let t = make_array_type (Typ0 INT) (getRandom 5) in
  match st.TryGetValue(t) with
  | (true, l) -> let n = get_random_elem l in (make_qualident n t depth st)
  | (false, _) -> st.Add(t, new List<name>([fresh_name st])) ; Qualident0 "TODO"
and get_bool (depth:int) (st:symtab) : qualident = Qualident0 "TODO"
and get_char (depth:int) (st:symtab) : qualident = Qualident0 "TODO"

and make_int_expr (depth:int) (st:symtab) : expr = Expr0 (make_int_simplexpr (depth-1) st)
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
  | 0 -> Factor0 (get_int (depth-1) st)
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
  | 0 -> Factor0 (get_bool (depth-1) st)
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

type stat = Stat0 of qualident * expr
          | Stat1 of call
          | Stat2 of expr * stat list * stat list
          | Stat3 of expr * stat list
          | Stat4 of expr option

exception Return_type_error

let rec make_stat (depth:int) (st:symtab) (rettyp : typ) : stat =
  match (getRandom 3) with
  | 0 -> Stat0 ((get_int (depth-1) st), (make_int_expr (depth-1) st))
  | 1 -> Stat0 ((get_bool (depth-1) st), (make_bool_expr (depth-1) st))
  | 2 -> Stat0 ((get_char (depth-1) st), (make_char_expr (depth-1) st))
  | 3 -> Stat1 (make_int_call (depth-1) st)
  | 4 -> Stat1 (make_bool_call (depth-1) st)
  | 5 -> Stat1 (make_char_call (depth-1) st) 
  | 6 -> Stat2 ((make_bool_expr (depth-1) st),
                [for x in 0 .. (getRandom depth) -> (make_stat (depth-1) st rettyp)],
                [for x in 0 .. (getRandom depth) -> (make_stat (depth-1) st rettyp)])
  | 7 -> Stat3 ((make_bool_expr (depth-1) st), [for x in 0 .. (getRandom depth) -> (make_stat (depth-1) st rettyp)])
  | _ -> match rettyp with
         | Typ0 NULL -> Stat4 None
         | Typ0 INT ->  Stat4 (Some (make_int_expr (depth-1) st))
         | Typ0 CHAR ->  Stat4 (Some (make_char_expr (depth-1) st))
         | Typ0 BOOL ->  Stat4 (Some (make_bool_expr (depth-1) st))
         | _ -> raise Return_type_error


type func = Func of typ * symtab * stat list
type md = Md of symtab * func list * stat list

let make_func (depth:int) (sn:int) : func =
  let t = (match (getRandom 4) with 0 -> Typ0 NULL | 1 -> Typ0 INT | 2 -> Typ0 CHAR | _ -> Typ0 BOOL) in
  let st = new_symtab() in
  Func (t, st, [for x in 1 .. sn -> make_stat depth st t])

let make_module (depth:int) (fn:int) (sn:int) : md =
  let st = new_symtab() in
  Md (st, [for x in 1 .. fn -> make_func depth sn], [for x in 1 .. sn -> make_stat depth st (Typ0 NULL)])