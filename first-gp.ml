type var = string

exception Length_error of string;;
  
type exp =
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Div of exp * exp
  | Int of int
  | Var of var

let term_set = [Int 2; Int 5; Var "x"; Var "y"]
let func_set = ["Plus"; "Minus"; "Times"; "Div"]

let rec nth n list = match (n, list) with
  | (_,[]) -> raise (Length_error "cannot get nth value from empty list")
  | (0, h :: t) -> h
  | (n, h :: t) -> nth (n-1) t
    
let rec length list = match list with
  | [] -> 0
  | h :: t -> 1 + (length t)

let choose_random_element list = nth (Random.int (length list)) list
				   		 
let rec gen_rnd_expr func_set term_set max_d methd = match max_d with
  | 0 -> let expr = choose_random_element(term_set) in expr
  | x -> let func = choose_random_element(func_set) in
	 match func with
	 | "Plus" -> Plus(gen_rnd_expr func_set term_set (x-1) methd,gen_rnd_expr func_set term_set (x-1) methd)
	 | "Minus"-> Minus(gen_rnd_expr func_set term_set (x-1) methd,gen_rnd_expr func_set term_set (x-1) methd)
	 | "Times"-> Times(gen_rnd_expr func_set term_set (x-1) methd,gen_rnd_expr func_set term_set (x-1) methd)
	 | "Div"  -> Div(gen_rnd_expr func_set term_set (x-1) methd,gen_rnd_expr func_set term_set (x-1) methd)
							   
	 
		  
				 
					   
	 
	 
							    
	   
			  
