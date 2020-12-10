(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = let (a,b,c) = tup in (c,b,a)

let abs x = if x >= 0 then x else x * -1

let area x y = 
   let (x1, x2) =  x in
   let (y1, y2) = y in
   (y1 - x1) * (y2 - x2) ;;

let volume x y = 
   let (x1, x2, x3) =  x in
   let (y1, y2, y3) = y in
   (y1 - x1) * (y2 - x2) * (y3 - x3) ;;

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec factorial x = if x = 0 || x = 1 then 1 else x * factorial(x-1)

let rec pow x y = if y = 0 then 1 else x * pow (x) (y-1)

let rec log x y = if (y / x) >= 1 then 1 + log x (y / x) else 0

let rec is_prime_helper x count =
   if x <= 1 || (count < x && (x mod count) = 0) then false
   else if count <= x && (x mod count) != 0 then is_prime_helper x (count + 1)
   else true
   
let rec is_prime x = is_prime_helper x 2

let rec next_prime x = if is_prime x = true then x else next_prime (x+1)

(*****************)
(* Part 3: Lists *)
(*****************)

let rec len lst =
   match lst with
   [] -> 0
   |h::t -> 1 + len t

let rec get_helper idx lst c = 
   match lst with
   [] -> failwith "Out of bounds"
   |h::t -> if c = idx then h else (get_helper idx t (c+1))

let rec get idx lst = get_helper idx lst 0

let larger lst1 lst2 = 
   if (len lst1) = (len lst2) then []
   else if (len lst1) > (len lst2) then lst1
   else lst2

let rec reverse_helper lst newLst = 
   match lst with 
   [] -> newLst
   |h::t -> reverse_helper t (h::newLst)

let reverse lst = reverse_helper lst []

let rec combine_helper lst1 lst2 newLst =
   if lst2 != [] then 
      match lst2 with
      [] -> newLst
      |h::t -> combine_helper lst1 t (h::newLst)
   else if lst1 != [] then
      match lst1 with
      [] -> newLst
      |h::t -> combine_helper t lst2 (h::newLst)
   else
      newLst

let rec combine lst1 lst2 = combine_helper (reverse lst1) (reverse lst2) []


let rec rotate_helper1 idx lst newLst = 
   if idx = 0 then
      newLst 
   else
      let value = (get (idx - 1) lst) in
      rotate_helper1 (idx - 1) lst (value::newLst)

let rec rotate_helper2 idx lst newLst = 
   if idx = 0 then
      newLst
   else
      match lst with
      [] -> newLst
      |h::t -> rotate_helper2 (idx - 1) t (h::newLst)

let rec rotate shift lst = 
   let idx = shift mod (len lst) in
   let idx2 = (len lst) - idx in

   rotate_helper2 idx2 (reverse lst) (rotate_helper1 idx lst [])