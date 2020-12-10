open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let count_occ lst target = 
   fold (fun a x ->
   if x = target then
      a + 1
   else
      a)
   (0) (lst)

let uniq lst = 
   fold_right (fun x a -> 
      if (count_occ a x) = 0 then
         x::a
      else
         a)

   (lst) ([])

let assoc_list lst = 
let uniqueLst = uniq lst in

   fold_right (fun x a -> (x, (count_occ lst x))::a) (uniqueLst) ([])

let ap fns args = 
   fold (fun a x -> a @ (map x args)) ([]) (fns)
   
