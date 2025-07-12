type 'a tree =
  | Lf
  | Br of 'a * 'a tree * 'a tree;;

  let tree =Br(3, Br(10, Br(30, Lf, Lf), Br(15, Lf, Lf))
 ,Br(20, Br(40, Lf,Lf ), Lf ));;

 let rec size tr =
  match tr with 
  | Lf -> 0
  | Br(x, l, r) -> 1 + size l + size r;; 
  
  size tree;;

  let rec total tr =
    match tr with 
    | Lf-> 0
    | Br (x, l, r) -> x + total l + total r;;

    let max a b=
    if a > b then a
    else b;;

    let rec max_dep tr=
    match tr with 
    | Lf -> 0
    |Br (x, l, r) -> 1+ max ( max_dep l) (max_dep r);;
    max_dep tree;;

    let rec insert n tr=
    match tr with 
    |Lf -> Br(n, Lf, Lf)
    |Br(x, l, r) as t ->
      if n> x then Br(x, l, insert n r)
      else if n < x then Br(x, insert n l, r)
      else t;;

      let rec to_list tr =
        match tr with 
        | Lf -> []
        | Br (x, l, r) -> to_list l @ [x] @ to_list r;;

        to_list tree;;
        let tree1 =  insert 2 tree;;
       to_list tree1;;

       let tr = Br (10 , Br(20, Br(40, Lf, Lf), Lf ), Br(30, Lf, Lf)) 
