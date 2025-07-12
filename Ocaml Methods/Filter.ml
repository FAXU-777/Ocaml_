  let filter f lst=
         let rec aux lst acc =
            match lst with 
            | [] -> List.rev acc
            | x :: xs -> 
               if f x then 
                  aux xs (x :: acc)
            else 
               aux xs acc
            in aux lst [];;