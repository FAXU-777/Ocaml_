let sum n =
  let rec aux n acc =
    if n = 0 then acc
    else aux (n - 1) (acc + n)
  in aux n 0;;
