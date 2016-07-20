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

(* generate initial population and calculate initial fitness *)

let sorted_population = sort_pop_by_fitness (map (calculate_fitness (-1.0) 1.0 0.1 (fun x -> x *. x +. x +. 1.0)) (generate_initial_pop 4 0.5 population))

let t_prob = 0.8
 
let sort_pop_by_fitness pop_list = List.sort (fun (x1,y1) (x2,y2) -> compare y1 y2) pop_list
	   
(* select_individual_from_sorted_population sorted_population (Random.float 1.0) 0.0 0 *)
	   
let rec select_individual_from_sorted_population sorted_pop random_number p k =
  match (p > random_number), sorted_pop with
  | true, h :: t -> h
  | false, h :: t ->
     select_individual_from_sorted_population t random_number (p +. ( t_prob *. (pow_float (1.0 -. t_prob) k) ) ) (k+1)
  | _, [] -> select_individual_from_sorted_population sorted_population (Random.float 1.0) 0.0 0

let rec pow_float float exp = match exp with
  | 0 -> 1.0
  | k -> float *. (pow_float float (k - 1))
				             
(* choose k (the tournament size) individuals from the population at random
choose the best individual from pool/tournament with probability p
choose the second best individual with probability p*(1-p)
choose the third best individual with probability p*((1-p)^2)
and so on... *)
					     
let rec tournament_selection k = match k with
  | 0 -> []
  | k -> (select_individual_from_sorted_population sorted_population (Random.float 1.0) t_prob 0) :: tournament_selection (k-1)

(* define reproduction, mutation and crossover functions *)

let reproduction individual = match individual with
  | (expr,fitness) -> expr

(* let rec subst (e,x : exp * var) (e' : exp) : exp = match e' with
  | Int z -> Int z
  | Float z -> Float z
  | Plus(e1, e2) -> Plus(subst (e,x) e1, subst (e,x) e2)
  | Minus(e1, e2) -> Minus(subst (e,x) e1, subst (e,x) e2)
  | Times(e1, e2) -> Times(subst (e,x) e1, subst (e,x) e2)
  | Div(e1, e2) -> Div(subst (e,x) e1, subst (e,x) e2)
  | Var y -> if y = x then e else Var y
 *)

exception Position_so_far of int
		   
let rec find_node_by_depth_first_search_and_mutate expr num = match num with
  | 1 -> (match expr with
	 | Var x -> choose_random_element term_set
	 | Int z -> choose_random_element term_set
	 | Float z -> choose_random_element term_set
	 | Plus(e1, e2) -> (let random_n = Random.float 1.0 in
			   match (random_n < 0.6666666666) with
			   |  true  -> (match (random_n < 0.3333333333) with
				       | true  -> Minus(e1, e2) 
				       | false -> Times(e1, e2)
				       )
		           |  false -> Div(e1,e2)		     
			   )
	 | Minus(e1, e2) -> (let random_n = Random.float 1.0 in
			   match (random_n < 0.6666666666) with
			   |  true  -> (match (random_n < 0.3333333333) with
				       | true  -> Plus(e1, e2) 
				       | false -> Times(e1, e2)
				       )
		           |  false -> Div(e1,e2)		     
			   )
  	 | Div(e1, e2) -> (let random_n = Random.float 1.0 in
			   match (random_n < 0.6666666666) with
			   |  true  -> (match (random_n < 0.3333333333) with
				       | true  -> Minus(e1, e2) 
				       | false -> Times(e1, e2)
				       )
		           |  false -> Plus(e1,e2)		     
			   )
  	 | Times(e1, e2) -> (let random_n = Random.float 1.0 in
			   match (random_n < 0.6666666666) with
			   |  true  -> (match (random_n < 0.3333333333) with
				       | true  -> Minus(e1, e2) 
				       | false -> Plus(e1, e2)
				       )
		           |  false -> Div(e1,e2)		     
			   )
	 )
  | k -> (match expr with
	  | Var x -> raise (Position_so_far k)
	  | Int z -> raise (Position_so_far k)
	  | Float z -> raise (Position_so_far k)
	  | Plus(e1,e2) -> (try Plus((find_node_by_depth_first_search_and_mutate e1 (k-1)), e2) with
			    | Position_so_far x -> Plus(e1, (find_node_by_depth_first_search_and_mutate e2 (x-1)) ))
	  | Minus(e1,e2) -> (try Minus((find_node_by_depth_first_search_and_mutate e1 (k-1)), e2) with
			    | Position_so_far x -> Minus(e1, (find_node_by_depth_first_search_and_mutate e2 (x-1)) ))
  	  | Times(e1,e2) -> (try Times((find_node_by_depth_first_search_and_mutate e1 (k-1)), e2) with
			    | Position_so_far x -> Times(e1, (find_node_by_depth_first_search_and_mutate e2 (x-1)) ))
	  | Div(e1,e2) -> try Div((find_node_by_depth_first_search_and_mutate e1 (k-1)), e2) with
			   | Position_so_far x -> Div(e1, (find_node_by_depth_first_search_and_mutate e2 (x-1)) )
	 )


let node_counter = ref 0
let parent1_node_counter = ref 0
let parent2_node_counter = ref 0
let reset_node_counter node_ref = (node_ref := 0)    
let reset_counter () = (node_counter := 0)
  
let rec num_nodes expr = match expr with
  | Var x -> node_counter := (!node_counter + 1)
  | Int z -> node_counter := (!node_counter + 1)
  | Float z -> node_counter := (!node_counter + 1)
  | Plus(e1,e2) -> node_counter := (!node_counter + 1); num_nodes e1; num_nodes e2
  | Minus(e1,e2) -> node_counter := (!node_counter + 1); num_nodes e1; num_nodes e2
  | Div(e1,e2) -> node_counter := (!node_counter + 1); num_nodes e1; num_nodes e2
  | Times(e1,e2) -> node_counter := (!node_counter + 1); num_nodes e1; num_nodes e2

let rec parent_num_nodes expr node_counter = match expr with
  | Var x -> node_counter := (!node_counter + 1)
  | Int z -> node_counter := (!node_counter + 1)
  | Float z -> node_counter := (!node_counter + 1)
  | Plus(e1,e2) -> node_counter := (!node_counter + 1); parent_num_nodes e1 node_counter; parent_num_nodes e2 node_counter
  | Minus(e1,e2) -> node_counter := (!node_counter + 1); parent_num_nodes e1 node_counter; parent_num_nodes e2 node_counter
  | Div(e1,e2) -> node_counter := (!node_counter + 1); parent_num_nodes e1 node_counter; parent_num_nodes e2 node_counter
  | Times(e1,e2) -> node_counter := (!node_counter + 1); parent_num_nodes e1 node_counter; parent_num_nodes e2 node_counter
				

let rec mutation individual = match individual with
  | (expr,fitness) -> num_nodes expr; let number_of_nodes = !node_counter in
		      let mutant_node_number = Random.int (number_of_nodes) in
		      find_node_by_depth_first_search_and_mutate expr (mutant_node_number + 1)

let mutate_and_reset_node_counter = (fun individual ->
    let mutated_individual = (mutation individual) in (reset_counter ()); mutated_individual)


let rec find_sub_expr expr num = match num with
  | 1 -> (match expr with
	  | _ -> expr
	 )
  | k -> (match expr with
	  | Var x -> raise (Position_so_far k)
	  | Int z -> raise (Position_so_far k)
	  | Float z -> raise (Position_so_far k)
	  | Plus(e1,e2) -> (try find_sub_expr e1 (k-1) with
			    | Position_so_far x -> find_sub_expr e2 (x-1) )
  	  | Minus(e1,e2) -> (try find_sub_expr e1 (k-1) with
			    | Position_so_far x -> find_sub_expr e2 (x-1) )
	  | Times(e1,e2) -> (try find_sub_expr e1 (k-1) with
			     | Position_so_far x -> find_sub_expr e2 (x-1) )
	  | Div(e1,e2) -> (try find_sub_expr e1 (k-1) with
			    | Position_so_far x -> find_sub_expr e2 (x-1) )
	 )

let rec replace_sub_expr expr replacement_sub_expr pos  = match pos with
  | 1 -> (match expr with
	  | x -> replacement_sub_expr
	 )
  | k -> (match expr with
	  | Var x -> raise (Position_so_far k)
	  | Int z -> raise (Position_so_far k)
	  | Float z -> raise (Position_so_far k)
	  | Plus(e1,e2) -> (try Plus((replace_sub_expr e1 replacement_sub_expr  (k-1)), e2) with
			    | Position_so_far x -> Plus(e1, (replace_sub_expr e2 replacement_sub_expr (x-1)) ))
  	  | Minus(e1,e2) -> (try Minus((replace_sub_expr e1 replacement_sub_expr  (k-1)), e2) with
			    | Position_so_far x -> Minus(e1, (replace_sub_expr e2 replacement_sub_expr (x-1)) ))
  	  | Div(e1,e2) -> (try Div((replace_sub_expr e1 replacement_sub_expr  (k-1)), e2) with
			   | Position_so_far x -> Div(e1, (replace_sub_expr e2 replacement_sub_expr (x-1)) ))
	  | Times(e1,e2) -> (try Times((replace_sub_expr e1 replacement_sub_expr  (k-1)), e2) with
			    | Position_so_far x -> Times(e1, (replace_sub_expr e2 replacement_sub_expr (x-1)) ))
	 )

let crossover parent1 parent2 = match parent1, parent2 with
  | (expr1,fitness1),(expr2,fitness2) -> parent_num_nodes expr1 parent1_node_counter; parent_num_nodes expr2 parent2_node_counter;
					 let number_of_nodes_parent1 = !parent1_node_counter in
					 let parent1_node_number = (Random.int (number_of_nodes_parent1) + 1) in
					 let parent1_sub_expr = find_sub_expr expr1 parent1_node_number in
					 let number_of_nodes_parent2 = !parent2_node_counter in
					 let parent2_node_number = (Random.int (number_of_nodes_parent2) + 1) in
					 let parent2_sub_expr = find_sub_expr expr2 parent2_node_number in
					 reset_node_counter parent1_node_counter; reset_node_counter parent2_node_counter;
					 let offspring1 = replace_sub_expr expr1 parent2_sub_expr parent1_node_number in
					 let offspring2 = replace_sub_expr expr2 parent1_sub_expr parent2_node_number in
					 [offspring1; offspring2]

let hd list = match list with
  | h :: t -> h

let tl list = match list with
  | h :: t -> t
  
let rec create_new_generation old_sorted_generation iterations = match iterations with
  | 0 -> []
  | k -> let random_number = Random.float 1.0 in
	 match (random_number > 0.9) with
	 | true -> let offspring = (tournament_selection 1) in
		   if random_number < 0.98 then (reproduction (hd(offspring))) :: (create_new_generation old_sorted_generation (iterations - 1))
		   else (reproduction (hd(offspring))) :: (create_new_generation old_sorted_generation (iterations - 1))
	 | false -> let offspring = (tournament_selection 2) in
		    (crossover (hd(offspring)) (hd(tl(offspring)))) @ (create_new_generation old_sorted_generation (iterations - 1))
									
						 
	 
	   
		      
		      
								     
							  
  
				   
	   



  
			


    
  

				    
			

			      
			      

			      
			      
    
    
    
    
    
      

  
  

		  


				 
					   
	 
	 
							    
	   
			  
