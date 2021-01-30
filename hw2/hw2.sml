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

(* problem 2b*)
fun card_value (_ : suit, rank) =
    case rank of
        Jack => 10
        | Queen => 10
        | King => 10
        | Ace => 11
        | Num v => v

(* problem 2c*)
fun remove_card (cs, c, e) =
    case cs of
        [] => raise e
        | h::cs' => if h = c then cs' else remove_card (cs', c, e)

(* problem 2d*)
fun all_same_color cs =
    case cs of
        [] => true
        |hd::[] => true
        |head::neck::_ => card_color head = card_color neck andalso all_same_color (tl cs)

(* problem 2e*)
fun sum_cards cs =
    let
        fun helper(cs, accu) =
            case cs of
                [] => accu
                | h::cs' => helper(cs', (card_value h) + accu)
    in
        helper(cs, 0)
    end

(* problem 2f*)
fun score (cs, g) =
    let 
        val sum = sum_cards cs
        val preliminary_score = if sum > g then 3 * (sum - g) else (g - sum)
    in
        if all_same_color cs then (preliminary_score div 2) else preliminary_score
    end

(* problem 2g*)
fun officiate (cs, ms, g) =
    let
        fun helper (cs, ms, g, hand) =
            case ms of
                [] => score(hand, g)
                | (Discard card)::ms' => helper(cs, ms', g, remove_card(hand, card, IllegalMove))
                | Draw::ms' => 
                    case cs of
                        [] => score(hand, g)
                        | card::cs' => 
                            let
                                val new_score = score(card::hand, g)
                            in
                                if  new_score > g then new_score 
                                else helper(cs', ms', g, card::hand)
                            end
    in
        helper (cs, ms, g, [] (*hand*))
    end
