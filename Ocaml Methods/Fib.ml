 let fib n =
         let rec aux n acc acc2=
         if n = 0  then acc
         else aux (n-1) acc2 (acc + acc2)
      in aux n 0 1;;


(*without acc *)
   let rec fib n =
      if n <3 then 1
      else fib(n-2) + fib(n-1);;

