type student_id = int
type grade = int
type final_grade = {id: student_id, grade: grade option}
datatype pass_fail = pass | fail

(* can i create a final_grade constructor in the repl? like final_grade(2, NONE) *)
fun pass_or_fail {grade, id} =  
    case grade of
        NONE => fail
    |SOME(i) => if i > 75 then pass else fail


fun has_passed final_grade = 
    case pass_or_fail final_grade of
        pass => true
    |fail => false


(*(pass_fail * final_grade) list -> int*)
(* val gs = [(pass, {id=2, grade=SOME(2)}),(fail, {id=3, grade=SOME(92)}), (pass, {id=4, grade=SOME(92)})]; *)
fun number_misgraded xs = 
    case xs of
        [] => 0
        |(pf, fg)::xs' =>  
            let val tl_number_misgraded = number_misgraded xs' 
            in
                case (pf, has_passed fg) of
                        (pass, false) => 1 + tl_number_misgraded
                        | (fail, true) => 1 + tl_number_misgraded 
                        | _ => tl_number_misgraded
            end
(* if number_misgraded gs = 2 then "test pass" else "test fail"; *)


datatype 'a tree = leaf
                    |node of {value: 'a, left: 'a tree, right: 'a tree}
datatype flag = leave_me_alone | prune_me

fun tree_height t =
    case t of
    leaf => 0
    |node{value = _, left = l, right = r} => Int.max(tree_height l, tree_height r) + 1

(*val mytree = node{value=3, left = node{value=2, left  = leaf, right=leaf}, right = leaf};*)

fun sum_tree t =
    case t of
    leaf => 0
    | node{value = v, left = l, right = r} => v + sum_tree l + sum_tree r

(*val mytree = node{value=leave_me_alone, left = node{value=leave_me_alone, left=leaf, right=node{value=leave_me_alone, left=leaf, right=node{value=prune_me, left=leaf, right=leaf}}}, right = leaf};*)
fun gardener t =
        case t of
        leaf => leaf
        | node{value = v, left = l, right = r} =>
            let 
                val l_pruned = gardener l;
                val r_pruned = gardener r;
            in 
                case v of
                leave_me_alone => node{value = v, left = l_pruned, right = r_pruned}
                |prune_me => leaf
            end

datatype nat = ZERO | SUCC of nat

fun is_positive x =
    case x of
    ZERO => false
    |SUCC(_) => true

exception Negative

fun pred x = 
    case x of
    ZERO => raise Negative
    | SUCC(x) => x

fun nat_to_int x =
    case x of
    ZERO => 0
    |SUCC v => 1 + nat_to_int v

fun int_to_nat x =
    case x of
    0 => ZERO
    |x => if x < 0 then raise Negative else SUCC(int_to_nat(x - 1))

fun add (a, b) = 
    case (a, b) of
    (ZERO, ZERO) => ZERO
    |(a, ZERO) => a
    |(a, SUCC(b')) => SUCC(add(a, b'))

fun sub (a, b) = 
    case (a, b) of
    (ZERO, ZERO) => ZERO
    |(a, ZERO) => a
    |(a, SUCC(b')) => pred(sub(a, b'))

fun mul (a, b) = case (a, b) of
    (ZERO, _) => ZERO
    |(_, ZERO) => ZERO
    |(a, SUCC(b')) => add(a, mul(a, b'))

fun less_than (a, b) = 
    case (a, b) of
    (ZERO, ZERO) => false
    |(a, ZERO) => false
    |(ZERO, b) => true
    |(SUCC(a'), SUCC(b')) => less_than(a', b')

datatype intSet = 
  Elems of int list (*list of integers, possibly with duplicates to be ignored*)
| Range of { from : int, to : int }  (* integers from one number to another *)
| Union of intSet * intSet (* union of the two sets *)
| Intersection of intSet * intSet (* intersection of the two sets *)

fun isEmpty x =
    case x of
    Elems(xs) => null xs
    | Range{from, to} => from >= to
    | Union(s1, s2) => isEmpty(s1) andalso isEmpty(s2)
    | Intersection(s1, s2) => isEmpty(s1) orelse isEmpty(s2)

