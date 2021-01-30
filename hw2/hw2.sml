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
fun get_substitutions1(sss, x) =
    case sss of
        [] => []
        | ss::sss' => 
            case all_except_option(x, ss) of
                NONE => get_substitutions1(sss', x)
                | SOME(lst) => lst @ get_substitutions1(sss', x)


(* problem 1c*)
fun get_substitutions2(sss, x) = 
    let
        fun helper(sss, x, accu) = 
            case sss of
                [] => accu
                | ss::sss' =>
                    case all_except_option(x, ss) of
                        NONE => helper(sss', x, accu)
                        | SOME(lst) => helper(sss', x, accu @ lst)
    in
        helper(sss, x, [])
    end


(* problem 1d*)
fun similar_names(sss, {first : string, middle : string, last : string}) =
    let
        val first_names = get_substitutions1(sss, first);
        fun get_names first_names = 
            case first_names of
                [] => []
                | fname::tl_fnames => {first=fname, middle=middle, last=last} :: (get_names tl_fnames)
    in
        {first=first, middle=middle, last=last} :: (get_names first_names)
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
(* problem 2a*)

fun card_color (suit, _ : rank) =
    case suit of
        Clubs => Black
        | Spades => Black
        | Diamonds => Red
        | Hearts => Red

fun card_value (_ : suit, rank) =
    case rank of
        Jack => 10
        | Queen => 10
        | King => 10
        | Ace => 11
        | Num v => v
    
fun remove_card (cs, c, e) =
    case cs of
        [] => raise e
        | h::cs' => if h = c then cs' else remove_card (cs', c, e)
