let abs x =
  if x >= 0 then x
  else (-x)

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup (tup : 'a * 'b) =
  match tup with
  | (a, b) -> (b, a)
let rev_triple (tup : 'a * 'b * 'c) =
  match tup with 
  | (a, b, c) -> (c, b, a)

let is_odd x = 
  x mod 2 = 1

let is_older (date1: int * int * int) (date2: int * int * int) = 
  (*year, month, day*)
  let (y1, m1, d1) = date1 in
  let (y2, m2, d2) = date2 in
  if (y1<y2 || (y1=y2 && (m1<m2 || (m1=m2 && d1<d2))))
    then true
  else
    false

let to_us_format (date1: int * int * int) = 
  match date1 with
  | (y, m, d) -> (m, d, y)
(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec pow x p = 
  if (p=1) then
    x
  else
    x * pow x (p-1)

let rec fac n =
  if (n=1) then
    n 
  else
    n * fac (n-1)

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get_nth ((idx:int), (lst: 'a list)) =
    match lst with
    | [] -> failwith "index out of bounds"
    | h :: t when idx == 0 -> h
    | h :: t -> get_nth (idx-1, t)

let larger lst1 lst2 =
  if ((List.length lst1) > (List.length lst2)) then
    lst1
  else if ((List.length lst1) < (List.length lst2)) then
    lst2
  else
    []
  
let rec singleListSum lst sumSoFar =
  match lst with
  | [] -> sumSoFar
  | h5 :: t5 -> singleListSum t5 (sumSoFar + h5)

let sum lst1 lst2 =
  let lst1Sum = (singleListSum lst1 0) in
  let lst2Sum = (singleListSum lst2 0) in
  lst1Sum + lst2Sum

