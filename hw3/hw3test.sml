
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
val test8c = all_answers (fn x => if x > 3 then SOME [x] else NONE) [3, 4, 5, 6, 7] = NONE


(* val p1 = Wildcard
val p2 = Variable "x"
val p3 = TupleP [p1, p2]
val p4 = ConstructorP ("anyX", p3) *)

val test9a = count_wildcards Wildcard = 1
val test9a1 = count_wildcards (ConstructorP ("any_x", TupleP [Wildcard, Variable "x"])) = 1
val test9a2 = count_wildcards (ConstructorP ("any_x", TupleP [Wildcard, Wildcard])) = 2

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b1 = count_wild_and_variable_lengths (Variable("ab")) = 2
val test9b2 = count_wild_and_variable_lengths (ConstructorP ("any_x", TupleP [Wildcard, Variable "abc"])) = 4

val test9c = count_some_var ("x", Variable("x")) = 1
val test9c1 = count_some_var ("x", TupleP [Variable("x"), Variable("x")]) = 2
val test9c2 = count_some_var ("x", TupleP [Wildcard]) = 0

val test10 = check_pat (Variable("x")) = true
val test10a = check_pat (TupleP [Variable("x"), Variable("x")]) = false
val test10b = check_pat (TupleP [Variable("x"), Variable("y")]) = true
val test10c = check_pat (TupleP [Variable("x"), Variable("y"), Variable("x")]) = false

val test11 = match (Const(1), UnitP) = NONE
val test11a = match (Const(1), ConstP 1) = SOME [] 
val test11b = match (Constructor("x", Const 1), Variable "x") = SOME [("x", Const 1)]

(*
val test12 = first_match Unit [UnitP] = SOME [] *)
