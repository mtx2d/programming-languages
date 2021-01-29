
val test1a = all_except_option("a", ["a", "a"]) = NONE
val test1b = all_except_option("a", []) = NONE
val test1c = all_except_option("a", ["b", "a", "c"]) = SOME(["b", "c"])
val test1d = all_except_option("", []) = NONE
val test1e = all_except_option("", ["a"]) = SOME(["a"]) 
val test1f = all_except_option("a", ["b", "a", "b"]) = SOME(["b", "b"])