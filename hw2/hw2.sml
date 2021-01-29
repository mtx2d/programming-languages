(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* problem 1a*)
fun all_except_option (s, ss) =
    let fun helper (s, ss) = 
            case ss of
                [] => (false, [])
                | cnt::ss' => 
                let 
                    val (has_s, lst) = helper(s, ss')
                in 
                    if same_string(s, cnt) then (true, lst)
                    else (false orelse has_s, cnt :: lst)
                end
    in
        case helper(s, ss) of
            (true, []) => NONE
            |(true, lst) =>  SOME(lst)
            |_ => NONE

    end

(* problem 1b *)
fun get_substitutions1(sss, s) =
    case sss of
        [] => []
        | hd_ss::sss' => []

        

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
