let rec cesar key l  = match l with
  |[] -> []
  |x::s -> char_of_int(int_of_char(x)+key mod (List.length s+1) )::cesar key s
;;
  cesar 4 ['a';'z';'b';'c'];;
let  list_of_string str =
  let rec aux acc l =
    if acc < 0 then l else aux (acc-1) (str.[acc]::l) in
 aux (String.length str-1) []                                     
;;
let rec output_char_list out c = match c with
  |[] -> output_char out '\n'
  |x::s -> begin output_char out x; output_char_list out s end
;;

  
let rec chiffrement_canal key iN out  =
  let lit =  input_line iN in
  let chiffre = cesar key (list_of_string lit) in
  output_char_list out chiffre;
  chiffrement_canal key iN out;
;;

  
let iN = open_in "/home/mrstruct/Desktop/L3/PFA/TP/livre.txt";;
let out = open_out "/home/mrstruct/Desktop/L3/PFA/TP/fichier.txt";;
 chiffrement_canal 4 iN out;;
      close_out out;;
        close_in iN;;
        
