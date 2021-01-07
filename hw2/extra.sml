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