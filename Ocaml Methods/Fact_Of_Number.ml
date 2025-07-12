let rec fact n =
   if n = 0 then 1 
   else n * fact(n-1);;


(* with accumulator*)
 let fact n =
      let rec aux n acc =
         if n = 0 then acc
         else aux (n-1) (acc * n )
      in
      aux n 1;;