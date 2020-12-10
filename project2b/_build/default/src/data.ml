open Funs

(************************)
(* Part 2: Integer BSTs *)
(************************)

type int_tree =
 |IntLeaf
 |IntNode of int * int_tree * int_tree

let empty_int_tree = IntLeaf

let rec int_insert x t =
  match t with
 |IntLeaf -> IntNode(x, IntLeaf, IntLeaf)
 |IntNode (y, l, r) when x > y -> IntNode (y, l, int_insert x r)
 |IntNode (y, l, r) when x = y -> t
 |IntNode (y, l, r) -> IntNode (y, int_insert x l, r)

let rec int_mem x t =
  match t with
 |IntLeaf -> false
 |IntNode (y, l, r) when x > y -> int_mem x r
 |IntNode (y, l, r) when x = y -> true
 |IntNode (y, l, r) -> int_mem x l

(* Implement the functions below. *)

let rec int_size t = 
  match t with
  |IntLeaf -> 0
  |IntNode(y, l, r) -> 1 + int_size l + int_size r

let rec int_max t = 
  match t with
  |IntLeaf -> raise (Invalid_argument("int_max"))
  |IntNode (y, l, r) -> if r != IntLeaf then int_max r else y

(****************************)
(* Part 3: Polymorphic BSTs *)
(****************************)

type 'a atree =
 |Leaf
 |Node of 'a * 'a atree * 'a atree
type 'a compfn = 'a -> 'a -> int
type 'a ptree = 'a compfn * 'a atree

let empty_ptree f : 'a ptree = (f,Leaf)

(* Implement the functions below. *)

let rec pinsert_helper x tree cmpFn = 
  match tree with
    |Leaf -> Node(x, Leaf, Leaf)
    |Node(a, l, r) when (cmpFn x a = 0) -> tree
    |Node(a, l, r) when (cmpFn x a > 0) -> Node(a, l, pinsert_helper x r cmpFn)
    |Node(a, l, r) -> Node(a, pinsert_helper x l cmpFn, r)

let pinsert x t = 
  let (compfn, tree) = t in 
  let atree = pinsert_helper x tree compfn in
  (compfn, atree)

let rec pmem x t = 
  let (compfn, tree) = t in  
  match tree with
  |Leaf -> false
  |Node(a, l, r) -> if (compfn x a = 0) then true 
                    else if (compfn x a > 0) then pmem x (compfn, r)
                    else pmem x (compfn, l)

let pinsert_all lst t = 
  let (compfn, tree) = t in fold_right pinsert lst (compfn, tree)

let rec list_helper tree = 
  match tree with
  |Leaf -> []
  |Node(a, l, r) -> list_helper l @ a::list_helper r

let rec p_as_list t = 
  let (compfn, tree) = t in list_helper tree
  

let pmap f t = 
  let (compfn, tree) = t in
  let lst = map f (p_as_list t) in pinsert_all lst (compfn, Leaf)


(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = 
  |Empty
  |Scope of (string * int) list * lookup_table
  
let empty_table () : lookup_table = Empty

let push_scope (table: lookup_table) : lookup_table = 
  Scope([], table)

let pop_scope (table: lookup_table) : lookup_table = 
  match table with
  |Empty -> failwith "No scopes remain!"
  |Scope(lst, t) -> t

let add_var name value (table: lookup_table) : lookup_table = 
  match table with
  |Empty -> failwith "There are no scopes to add a variable to!"
  |Scope(lst, t) -> Scope(((name, value)::lst), t)

let rec in_list lst name : int option =
  match lst with
  |[] -> None
  |(n, v)::t -> if n = name then Some v else in_list t name

let rec lookup name (table: lookup_table) = 
  match table with
  |Empty -> failwith "Variable not found!"
  |Scope(lst, t) -> match (in_list lst name) with
                    |None -> lookup name t
                    |Some(value) -> value

(*******************************)
(* Part 5: Shapes with Records *)
(*******************************)

type pt = { x: int; y: int };;
type shape =
 |Circ of { radius: float; center: pt }
 |Square of { length: float; upper: pt }
 |Rect of { width: float; height: float; upper: pt }
;;

(* Implement the functions below. *)

let area s =
match s with 
|Circ({radius; _}) ->  radius *. radius *. 3.14
|Square({length; _}) -> length *. length
|Rect({width; height; _}) -> width *. height

let filter f lst = 
fold (fun a x -> if f x = true then x::a else a) [] lst
