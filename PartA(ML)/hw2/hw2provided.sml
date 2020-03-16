(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* func #1 *)
(* two not-matched situations have to be justify: *)
(* 1. x not matched, no strings after it => return NONE *)
(* 2. x not matched, hving strings after it => have to be concatenated with former list => return SOME (x::res) *)
fun all_except_option (s : string, s_list : string list) =
    case s_list of
	[] => NONE
      | x::res => if same_string (x, s)
		  then SOME res
		  else case all_except_option (s, res) of
			   NONE => NONE
			 | SOME tail => SOME(x::tail)

(* func #2 *)
fun get_substitutions1 (s_list_list : string list list, s : string) =
    case s_list_list of
	[] => []
      | x_list :: res_list => case all_except_option (s, x_list) of
				 NONE => get_substitutions1(res_list, s)   (* not all the ways lead to recursion *)
			       | SOME sublist => sublist @ get_substitutions1(res_list, s)

(* func #3 *)
(* tail recursion *)
fun get_substitutions2 (s_list_list : string list list, s : string) =
    let fun aux (s_list_list, s, acc) =
	    case s_list_list of
		[] => acc
	      | x_list :: res_list => case all_except_option (s, x_list) of
					 NONE => aux (res_list, s, acc)
				       | SOME sublist => aux (res_list, s, acc @ sublist)
    in
	aux (s_list_list, s, [])
    end	     

(* func #4 *)
fun similar_names (s_list_list : string list list, name : {first:string, last:string, middle:string}) =
    let fun aux (sublist, name) =
	    case sublist of
		[] => []
	      | sub::res_list => case name of
				     {first=x, last=y, middle=z} => [{first=sub, last=y, middle=z}] @ aux(res_list, name)
    in
	case name of
	    {first=x, last=y, middle=z} =>
	    (* unable to use "first" to inference *)
	    name::aux(get_substitutions2(s_list_list, x), name)
    end

		 
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)  

(* func #5 *)
(* make use of type representation *)
fun card_color (c : card) =
    case c of
	(Clubs, _) => Black
      | (Spades, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red

			 
(* func #6 *)
fun card_value (c : card) =
    case c of
	(_, Num(idx)) => idx    (* remember to add the idx as the param *)
      | (_, Ace) => 11
      | _ => 10  (* everything else means "_" *)

(* func #7 *)
fun remove_card (cs : card list, c : card, e : exn) =
    let fun aux (cs, c, e, flag) =
	    case cs of
		[] => if flag then [] else raise e   (* the only condition to raise exn *)
	      | cd::cds => if c = cd andalso flag = false  (* only remove first one *)
			   then aux(cds, c, e, true)
			   else cd::aux(cds, c, e, flag)
    in
	aux(cs, c, e, false)
    end

(* func #8 *)
(* make use of card_color defined in func #5 *)
fun all_same_color (cs : card list) =
    case cs of
	[] => true
      | cd::[] => true
      (* when meaning 2 and more remains, we have to introduce 3 *)
      | cd1::cd2::cds => card_color(cd1) = card_color(cd2) andalso all_same_color(cd2::cds)

(* func #9 *)
fun sum_cards (cs : card list) =
    let fun aux (cs, acc) =
	    case cs of
		[] => acc
	      | cd::cds => aux(cds, acc+card_value(cd))
    in
	aux (cs, 0)
    end

(* func #10 *)
fun score (cs : card list, goal : int) =
    let    (* bind the local variables first *)
	val sum = sum_cards(cs)
	val prelim_scr = if sum > goal
			 then 3 * (sum - goal)
			 else goal - sum
    in
	if all_same_color(cs)
	then prelim_scr div 2
	else prelim_scr
    end

(* func #11 *)
fun officiate (cs : card list, mvs : move list, goal : int) =
    let fun round (held, goal, cds, mv) =
	    if sum_cards(held) > goal
	    then score(held, goal)     (* Game over if held score > goal *)
	    else case mv of
		     [] => score(held, goal)     (* Game over if no move *)
		   | mv'::mvs => case mv' of
				     Discard(cd) => round(remove_card(held, cd, IllegalMove), goal, cds, mvs)    (* Discard option *)
				   | Draw => case cds of    (* Draw option *)
						 [] => score(held, goal)
					       | cd::[] => round(cd::held, goal, [], mvs)
					       | cd::cds' => round(cd::held, goal, cds', mvs)
    in
	round ([], goal, cs, mvs)
    end
	
