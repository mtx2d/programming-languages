(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
(* problem 1*)
fun only_capitals ss = List.filter (fn x => Char.isUpper (String.sub (x, 0))) ss

(* problem 2*)
fun longest_string1 ss = foldl (fn (x, y) => if String.size x > String.size y then x else y) "" ss

(* problem 3*)
fun longest_string2 ss = foldl (fn (x, y) => if String.size x >= String.size y then x else y) "" ss

(* problem 4*)
fun logest_string_helper p ss =
    foldl (fn (x, y) => if p (String.size x, String.size y) then x else y) "" ss

val longest_string3 = logest_string_helper (fn (x, y) => x > y)

val longest_string4 = logest_string_helper (fn (x, y) => x >= y)
