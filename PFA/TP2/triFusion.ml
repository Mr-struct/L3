let rec couper l = match l with
    | [] -> ([],[])
    | [x] -> ([x],[])
    | x1::x2::s->let(g,d) = couper s in
                 (x1::g,x2::d)
;;
  
let l = [1;2;3;4];;
  couper l;;
    
    
