
(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1a = only_capitals ["Aa","Bb","Cc", "d"] = ["Aa","Bb","Cc"]

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2a = longest_string1 [] = ""
val test2c = longest_string1 ["", "c"] = "c"
val test2d = longest_string1 ["aa", "cc"] = "aa"

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3a = longest_string2 ["aa","cc", "dd"] = "dd"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"
val test5a = longest_capitalized [] = ""

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8a = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []
val test8b = all_answers (fn x => if x > 3 then SOME [x] else NONE) [4, 5, 6, 7] = SOME [4,5,6,7]
(*

 val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME [] *)
