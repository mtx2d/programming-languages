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

(* problem 5*)
val longest_capitalized = longest_string3 o only_capitals

(* problem 6*)
val rev_string = String.implode o List.rev o String.explode 

(* problem 7*)
fun first_answer f xs =
    case xs of 
        [] => raise NoAnswer
        |h::xs' => case f h of
                        SOME(v) => v
                        | NONE => first_answer f xs'

(* problem 8*)
fun all_answers f xs =
    let
        fun helper f xs accu =
            case xs of
                [] => SOME accu
                |h::xs' =>  case f h of
                                NONE => NONE
                                |SOME x => helper f xs' (accu @ x)
    in
        helper f xs []
    end


(* problem 9a*)
val count_wildcards = g (fn _ => 1) (fn _ => 0) 

(* problem 9b*)
val count_wild_and_variable_lengths = g (fn _ => 1) (fn name => String.size name)

(* problem 9c*)
fun count_some_var (s, p) =
    g (fn _ => 0) (fn name => if s = name then 1 else 0) p