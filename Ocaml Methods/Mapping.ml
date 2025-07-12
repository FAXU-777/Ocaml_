  let map f lst =
  let rec aux l acc =
    match l with
    | [] -> List.rev acc
    | x::xs -> aux xs (f x :: acc)
  in
  aux lst []
;;