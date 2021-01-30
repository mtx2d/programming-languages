
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
val test2ab = card_color (Spades, Num 12) = Black
val test2ac = card_color (Diamonds,Num 12) = Red
val test2ad = card_color (Hearts,Num 12) = Red

