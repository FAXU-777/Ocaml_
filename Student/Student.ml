type student ={
  name : string;
  last_name : string;
  id : int; 
  current_semester : int ;
  grade : (int * int ) list ;
};;

let student1 = {
  name = "Nika";
  last_name ="pakhu";
  id = 777;
  current_semester = 3;
  grade = [(1, 100); (2,90); (3, 80)];
};;

let student2 = {
  name ="Shota";
  last_name = "shala";
  id = 123;
  current_semester = 3;
  grade = [ (1, 100); (2, 70); (3, 20)];
};;

type database = student list;;

let db : database = [];;
let  insert st db = st :: db;;

let db = insert student1 db ;;
let db = insert student2 db;;

let rec find_by_id id db =
  match db with 
  | [] -> []
  | x :: xs -> 
    if x.id = id then [x]
    else find_by_id id xs;;

    find_by_id 123 db;;
    find_by_id 777 db;;

let rec find_by_last_name last_name db =
  match db with 
  | [] -> []
  | x :: xs -> 
    if x.last_name =last_name then [x]
    else find_by_last_name last_name xs;;

    find_by_last_name "pakhu" db;;
    find_by_last_name "shala" db;;

    let rec remove id db =
      match db with 
      | [] -> []
      | x :: xs -> 
        if x.id = id then xs
        else remove id xs;;

        remove 123 db;;


let rec count sem db =
  match db with 
  | [] -> 0
  | x :: xs -> 
    if x.current_semester = sem then 1 + count sem xs
    else count sem xs;;

    count 3 db;;