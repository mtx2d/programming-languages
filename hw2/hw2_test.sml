
val test1aa = all_except_option("a", ["a"]) = SOME([])
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

(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = all_except_option ("string", ["string"]) = SOME []

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
             
             