   let remove n lst =
            let rec aux lst acc =
               match lst with 
               | [] -> acc
               | x :: xs -> 
                  if x = n then aux xs acc
                  else  aux xs (x :: acc)
               in aux lst [];; 