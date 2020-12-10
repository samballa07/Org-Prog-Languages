open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i]::l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
 match nfa with
  |{sigma = _; qs = qs1; q0 = first; fs = fs; delta = d} -> 
  List.fold_left (fun a x -> let (q, s1, q1) = x in 
    if List.mem q qs && (s1 = s) && (not (List.mem q1 a)) then q1::a else a)
    [] d

let rec eclosure_helper nfa qs lst = 
  match qs with 
   |[] -> lst
    |h::t -> 
    if (List.mem h lst) then (eclosure_helper nfa t lst)
    else eclosure_helper nfa (union t (move nfa [h] None)) (insert h lst)

let rec e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  eclosure_helper nfa qs []

 let rec accept_helper nfa curr charLst =
    match charLst with
   |[] -> curr
    |h::t -> accept_helper nfa (e_closure nfa (move nfa curr (Some h))) t;;

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let ans = accept_helper nfa (e_closure nfa [nfa.q0]) (explode s) in
  if intersection ans nfa.fs = [] then false else true;;

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  List.fold_left (fun a x -> insert (e_closure nfa (move nfa qs (Some x))) a)
  [] nfa.sigma

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  fold_left (fun a x -> 
  insert (qs, (Some x), e_closure nfa (move nfa qs (Some x))) a) [] nfa.sigma 

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  List.fold_right (fun x a -> if elem x qs then qs::a else a) nfa.fs []

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
  (work: 'q list list) : ('q list, 's) nfa_t =
  match work with
  |[] -> dfa
  |h::t -> let new_dfa = {
  sigma = dfa.sigma;
  qs = insert h dfa.qs ;
  q0 =  dfa.q0;
  fs = union (new_finals nfa h) dfa.fs;
  delta = union dfa.delta (new_trans nfa h);
  } in 
  nfa_to_dfa_step nfa new_dfa (diff (union (new_states nfa h) t) new_dfa.qs)

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let dfa = {
  sigma = nfa.sigma;
  qs = [(e_closure nfa [nfa.q0])];
  q0 =  (e_closure nfa [nfa.q0]);
  fs = [];
  delta = [];
} in
  nfa_to_dfa_step nfa dfa [e_closure nfa [nfa.q0]]
