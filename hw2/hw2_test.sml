
val test1a = all_except_option("a", ["a", "a"]) = NONE
val test1b = all_except_option("a", []) = NONE
val test1c = all_except_option("a", ["b", "a", "c"]) = SOME(["b", "c"])
