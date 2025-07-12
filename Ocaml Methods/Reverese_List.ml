 let rec rev lst =
         match lst with 
         |[] -> []
         | x :: xs -> rev xs @  [x];;


(* with accumulator *)
  let rev lst =
            let rec aux lst acc =
               match lst with 
               | [] -> acc
               | x :: xs -> aux xs ( x :: acc) 
            in 
            aux lst [];;
