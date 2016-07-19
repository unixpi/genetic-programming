type var = string

exception Error of string
exception Length_error of string;;
  
type exp =
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Div of exp * exp
  | Int of int
  | Float of float
  | Var of var

(* first step - identify terminal set *)	     
let term_set = [Var "x"; Var "x"; Var "x"; Var "x"; Var "x";
		Var "x"; Var "x"; Var "x"; Var "x"; Var "x";
		Float 0.0; Float 1.0; Float 2.0; Float 3.0; Float 4.0; Float 5.0;
		Float (-1.0); Float (-2.0); Float (-3.0); Float (-4.0); Float (-5.0)] 

(* second step - identify function set *)		 
let func_set = ["Plus"; "Minus"; "Times"; "Div"]

let generateRandomFloat bound =
  let sign = Random.int 2 in
  let float = Random.float bound in
  match sign with
  | 0 -> float
  | 1 -> (-. float)

let rec nth n list = match (n, list) with
  | (_,[]) -> raise (Length_error "cannot get nth value from empty list")
  | (0, h :: t) -> h
  | (n, h :: t) -> nth (n-1) t
    
let rec length list = match list with
  | [] -> 0
  | h :: t -> 1 + (length t)

let choose_random_element list = nth (Random.int (length list)) list

let rec gen_rnd_expr func_set term_set max_d methd =
  if (methd = "grow" &&
	((Random.float 1.0) < (float_of_int(length term_set) /. float_of_int((length term_set) + (length func_set)))))
  then choose_random_element(term_set)
  else match max_d with
       | 0 -> choose_random_element(term_set)
       | x -> let func = choose_random_element(func_set) in
	      match func with
	      | "Plus" -> Plus(gen_rnd_expr func_set term_set (x-1) methd,gen_rnd_expr func_set term_set (x-1) methd)
	      | "Minus"-> Minus(gen_rnd_expr func_set term_set (x-1) methd,gen_rnd_expr func_set term_set (x-1) methd)
	      | "Times"-> Times(gen_rnd_expr func_set term_set (x-1) methd,gen_rnd_expr func_set term_set (x-1) methd)
	      | "Div"  -> Div(gen_rnd_expr func_set term_set (x-1) methd,gen_rnd_expr func_set term_set (x-1) methd)

let rec subst (e,x : exp * var) (e' : exp) : exp = match e' with
  | Int z -> Int z
  | Float z -> Float z
  | Plus(e1, e2) -> Plus(subst (e,x) e1, subst (e,x) e2)
  | Minus(e1, e2) -> Minus(subst (e,x) e1, subst (e,x) e2)
  | Times(e1, e2) -> Times(subst (e,x) e1, subst (e,x) e2)
  | Div(e1, e2) -> Div(subst (e,x) e1, subst (e,x) e2)
  | Var y -> if y = x then e else Var y

let combineInts v1 v2 op = match v1, v2 with
  | Int i1, Int i2 -> Int (op i1 i2)
  | _, _ -> raise (Error "either one or both arguments given to combineInts are (is) not (an) Int(s)" )

let combineFloats v1 v2 op = match v1, v2 with
  | Float i1, Float i2 -> Float (op i1 i2)
  | _, _ -> raise (Error "either one or both arguments given to combineFloats are (is) not (an) Float(s)")							
let rec eval (e : exp) : exp = match e with
  | Int n -> Int n
  | Float n -> Float n
  | Plus(e1, e2) -> (let v1 = eval e1 in
		       let v2 = eval e2 in
		       try
			 combineFloats v1 v2 (+.)
		       with
			 _ -> combineInts v1 v2 (+)
		      )
    | Minus(e1, e2) -> (let v1 = eval e1 in
		       let v2 = eval e2 in
		       try
			 combineFloats v1 v2 (-.)
		       with
			 _ -> combineInts v1 v2 (-)
		       )
    | Times(e1, e2) -> (let v1 = eval e1 in
		       let v2 = eval e2 in
		       try
			 combineFloats v1 v2 ( *. )
		       with
			 _ -> combineInts v1 v2 ( * )
		       )
    | Div(e1, e2) ->  (let v1 = eval e1 in
		       let v2 = eval e2 in
		       match v2 with
		       | Float 0.0 -> Float 1.0
		       | Int 0 -> Int 1
		       | _ -> try
			      combineFloats v1 v2 ( /. )
			    with
			      _ -> combineInts v1 v2 ( / )
		      )
    | Var x -> raise (Error "unbound variable")

(* test cases

eval (subst(Int 4, "y") ((subst(Int 3, "x") (gen_rnd_expr func_set term_set 2 "grow"))));;
eval (subst(Int 4, "y") ((subst(Int 3, "x") (gen_rnd_expr func_set term_set 2 "full"))));;

calculate_fitness (gen_rnd_expr func_set term_set 2 "full") (-1.0) 1.0 0.1
 *)
(* Fitness : sum of absolute errors for x in {-1.0, -0.9, ...0.9, 1.0}
 *)

let epsilon = 1.0e-10
let (=.) a b = (abs_float (a-.b)) < epsilon;;
  
let calculate_fitness lbound ubound step ideal = (* return tuple (expr * total_fitness) *)
  (fun expr ->
  let rec calculate_fitness' current acc  =
    if current =. (ubound +. step) then (expr,acc) else match current with
					      | _ ->  let program_result = (eval (subst(Float current, "x") expr)) in
						      let ideal_result = Float (ideal(current)) in
						      let fitness = (eval (Minus(program_result,ideal_result))) in
						      match fitness with
						      | Float f -> let abs_f = abs_float(f) in
								   calculate_fitness' (current +. step) (acc +. abs_f)
  in calculate_fitness' lbound 0.0
  )
let population = []
		 

let rec generate_initial_pop number grow_percentage acc = match number with
  | 0 -> acc
  | _ -> if (Random.float 1.0) > grow_percentage then
	   let member = (gen_rnd_expr func_set term_set 2 "full") in
	   generate_initial_pop (number - 1) grow_percentage (member :: acc)
	 else
	   let member = (gen_rnd_expr func_set term_set 2 "grow") in
	   generate_initial_pop (number - 1) grow_percentage (member :: acc)

let rec map f l = match l with
  | [] -> []
  | h :: t -> (f h) :: (map f t);;

(* generate initial population and calculate initial fitness 
sort_pop_by_fitness (map (calculate_fitness (-1.0) 1.0 0.1 (fun x -> x *. x +. x +. 1.0)) (generate_initial_pop 4 0.5 population))
 *)

let sort_pop_by_fitness pop_list = List.sort (fun (x1,y1) (x2,y2) -> compare y1 y2) pop_list
				   
let tournament_selection size p = "placeholder" 
  
			

			      
			      

			      
			      
    
    
    
    
    
      

  
  

		  


				 
					   
	 
	 
							    
	   
			  
