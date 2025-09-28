let abs x =
  if x >= 0 then x
  else (-x)

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup (tup : 'a * 'b) = 
  (snd tup, fst tup);;
  
let rev_triple (tup : 'a * 'b * 'c) =
  let (fst, snd, thd) = tup in
  (thd, snd, fst)
;;
  



let is_odd x = 
  if x mod 2 = 0 then false
  else true;;


let is_older (date1: int * int * int) (date2: int * int * int) = 
  let (year1, month1, day1) = date1 and (year2, month2, day2) = date2 in

  if year1 < year2 then true
else if year2 < year1 then false
else if month1 < month2 then true
else if month2 <  month1 then false
else day1 < day2
;;




let to_us_format (date1: int * int * int) = 
  let (year, mon, day) = date1 in
  (mon, day, year);;
(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec pow x p = 
  if p = 0
    then 1
else
  x * pow x (p-1)
;;

let rec fac n = 
  if n = 0 || n =1 
    then 1
else
  n * fac(n-1)
;;


(*****************)
(* Part 3: Lists *)
(*****************)

let rec get_nth ((idx:int), (lst: 'a list)) = 
  match lst with
  | [] -> failwith "Index out of bounds"
  | x::xs -> 
    if idx = 0 then x
    else
      get_nth(idx-1,xs)
  


let larger lst1 lst2 =
  if List.length lst1 >= List.length lst2 then lst1 else lst2


let rec sum lst1 lst2 = 
  match (lst1, lst2) with
  | ([],[]) -> 0
  | (x::xs, []) -> x + sum xs []
  | ([] , y::ys) -> y + sum [] ys
  | (x::xs,y::ys) -> x+y + sum xs ys

  
