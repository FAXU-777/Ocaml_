  let prod lst =
      let rec aux lst acc=
      match lst with 
      | [] -> acc
      | x :: xs -> aux xs (x * acc)
   in 
   aux lst 1;;