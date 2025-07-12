   let sum lst =
  let rec aux lst acc =
    match lst with
    | [] -> acc
    | x::xs -> aux xs (acc + x)
  in
  aux lst 0;;
