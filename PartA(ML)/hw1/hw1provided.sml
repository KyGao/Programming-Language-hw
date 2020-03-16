(* assignment 1 *)
fun is_older (date1 : int * int * int, date2 : int * int * int) =
    if (#1 date1) <> (#1 date2)    (* evaluate the years *)
    then (#1 date1) < (#1 date2)
    else if (#2 date1) <> (#2 date2)    (* evaluate the months *)
    then (#2 date1) < (#2 date2)
    else (#3 date1) < (#3 date2)    (* evaluate the days *)

(* assignment 2 *)
fun number_in_month (date_list : (int * int * int) list, month : int) =
    if null date_list
    then 0
    else if  ((#2 (hd date_list)) = month)
    then 1 + number_in_month(tl date_list, month)
    else number_in_month(tl date_list, month)

(* assignment 3 *)
fun number_in_months (date_list : (int * int * int) list, month_list : int list) =
    if null month_list
    then 0
    else number_in_month(date_list, hd month_list) + number_in_months(date_list, tl month_list)

(* assignment 4 *)
fun dates_in_month (date_list : (int * int * int) list, month : int) =
    if null date_list
    then []
    else if ((#2 (hd date_list)) = month)
    then hd date_list :: dates_in_month(tl date_list, month)
    else dates_in_month(tl date_list, month)

(* assignment 5 *)
fun dates_in_months (date_list : (int * int * int) list, month_list : int list) =
    if null month_list
    then []
	     (* @ : append function that has type (α list * α list) → α list  *)
    else dates_in_month(date_list, hd month_list) @ dates_in_months(date_list, tl month_list)

(* assignment 6 *)
fun get_nth (string_list : string list, n : int) =
    if n = 1
    then (hd string_list)
    else get_nth(tl string_list, n-1)

(* assignment 7 *)
fun date_to_string (date : (int * int * int)) = 
(* try let expression *)
    let val month_str = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
    in get_nth(month_str, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

(* assignment 8 *)
fun number_before_reaching_sum  (sum : int, int_list : int list) =
(* Assume the entire list sums to more than the passed in value *)
    if sum <= (hd int_list)
    then 0
	     (* negative sign is unavailable *)
    else 1 + number_before_reaching_sum(sum + (~(hd int_list)), tl int_list)
				       
(*  assignment 9 *)
fun what_month (day : int) =
    let	val day_cnt = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    in 1 + number_before_reaching_sum(day, day_cnt)
    end

(* assignment 10 *)
fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

(* assignment 11 *)
fun oldest (date_list : (int * int * int) list) =
    (* try NONE and SOME options *)
    if null date_list
    then NONE
    else
        let
            val old = oldest(tl date_list)
        in
            if isSome old andalso is_older(valOf old, (hd date_list))
            then old
            else SOME (hd date_list)
        end		      
