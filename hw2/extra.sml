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
