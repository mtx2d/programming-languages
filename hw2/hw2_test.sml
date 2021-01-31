
val test1aa = all_except_option("a", ["a", "a"]) = NONE
val test1ab = all_except_option("a", []) = NONE
val test1ac = all_except_option("a", ["b", "a", "c"]) = SOME(["b", "c"])
val test1ad = all_except_option("", []) = NONE
val test1ae = all_except_option("", ["a"]) = NONE
val test1af = all_except_option("a", ["b", "a", "b"]) = SOME(["b", "b"])
val test1ag = all_except_option("a", ["b", "b", "b"]) = NONE

val test1ba = get_substitutions1(
                [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], 
                "Fred") = ["Fredrick", "Freddie", "F"]

val test1ca = get_substitutions2( [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred")
                                = ["Fredrick", "Freddie", "F"]

val test1da = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
            {first="Fred", middle="W", last="Smith"}) =  [{first="Fred", last="Smith", middle="W"},
                                                        {first="Fredrick", last="Smith", middle="W"},
                                                        {first="Freddie", last="Smith", middle="W"},
                                                        {first="F", last="Smith", middle="W"}]

val test2aa = card_color (Clubs, Jack) = Black
val test2ab = card_color (Spades, Num 10) = Black
val test2ac = card_color (Diamonds,Num 1) = Red
val test2ad = card_color (Hearts,Num 2) = Red

val test2ba = card_value (Clubs, Jack) = 10
val test2bb = card_value (Spades, Ace) = 11
val test2bc = card_value (Spades, Num 9) = 9

val test2ca = remove_card ([(Clubs, Num 1), (Clubs, Num 2)], (Clubs, Num 1), IllegalMove) = [(Clubs, Num 2)]
(* remove only the first one, if more than once*)
val test2cb = remove_card ([(Clubs, Num 1), (Clubs, Num 1)], (Clubs, Num 1), IllegalMove) = [(Clubs, Num 1)]
(* raise exception *)
val test2cc = (remove_card ([(Clubs, Num 1), (Clubs, Num 1)], (Clubs, Num 2), IllegalMove) handle IllegalMove => []) = []

val test2da = all_same_color [(Clubs, Ace)] = true
val test2db = all_same_color [(Clubs, Ace), (Spades, Ace)] = true
val test2dc = all_same_color [(Clubs, Ace), (Hearts, Ace)] = false
val test2dd = all_same_color [(Clubs, Ace), (Clubs, Ace), (Spades, Ace)] = true
val test2de = all_same_color [(Hearts, Ace), (Spades, Ace), (Spades, Ace)] = false
val test2df = all_same_color [(Clubs, Ace), (Clubs, Ace), (Hearts, Ace)] = false


val test2ea = sum_cards [(Clubs, Ace), (Spades, Ace)] = 22
val test2eb = sum_cards [(Clubs, Num 1), (Spades, Num 2)] = 3
val test2ec = sum_cards [(Clubs, Jack), (Spades, Num 2)] = 12
val test2ed = sum_cards [] = 0

val test2fa = score ([(Clubs, Ace), (Hearts, Ace)] (* 22 *), 0) = 66 
val test2fb = score ([(Clubs, Ace), (Hearts, Ace)], 23) = 1
val test2fc = score ([(Clubs, Ace), (Clubs, Ace)], 23) = 0
val test2fd = score ([(Clubs, Ace), (Clubs, Ace)], 22) = 0

val test3a = score_challenge ([(Clubs, Ace)], 12) = 1