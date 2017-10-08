let trois_ou_plus l  =
  match l with
  |[] -> false
  |[x]-> false 
  |[y;s] -> false
  |_ -> true
;;

let listVide = [];;
let list3 = [1;2;3;3];;
let list2 = [1;2];;
trois_ou_plus listVide;;
trois_ou_plus list2;;
trois_ou_plus list3;;
exception List_Vide;;

let rec dernier_element l = match l with
    [] -> raise List_Vide
   |[x] -> x
   |x::s -> dernier_element s
;;
  (* dernier_element list2;;*)
  
let rec somme l = match l with
    [] -> 0
   |x::s -> x + somme l
;;
let rec est_croissante l = match l with
    [] -> true
   |[x] -> true
   |x::m::s -> if x < m then est_croissante (m::s) else false
;;
  est_croissante list3;;
  
let rec nb_occ e l = match l with
    [] -> 0
   |x::s -> if e = x then 1 + nb_occ e s else nb_occ e s
;;

  nb_occ 3 list3;;
let rec nieme n l = match l with
    [] -> raise List_Vide
   |x::s -> if n = 0 then x else nieme (n-1) s
;;
  nieme 2 list3;;

let max l =
  let rec aux max l = match l with
      [] -> raise List_Vide
     |[x] -> if x >= max then x else max     
    |x::s -> if x > max then aux x s else aux max s
  in
  aux 0 l
;;
  max list3;;
    
  
