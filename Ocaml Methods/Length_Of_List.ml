 let rec len lst =
               match lst with 
               | [] -> 0
               | x :: xs -> 1 + len xs;;

(*with accumulato*)

  let length lst =
   let rec aux lst acc =
      match lst with 
      | [] -> acc
      | x :: xs -> aux xs (1 + acc)
   in 
   aux lst 0;;  