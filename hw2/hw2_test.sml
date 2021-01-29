
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