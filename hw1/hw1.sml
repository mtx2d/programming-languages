fun is_older ((y1, m1, d1), (y2, m2, d2)) = 
    if y2 > y1 then true 
    else if m2 > m1 then true 
    else if d2 > d1 then true 
    else false

(* (int * int * int) list * int*)
fun number_in_month (dates, target_month) =
    if dates = [] then 0
    else
        let
            val (year, month, day) = hd dates
            val tl_number_in_month = number_in_month(tl dates, target_month)
        in
            if month = target_month then
                1 + tl_number_in_month
            else
                tl_number_in_month
        end
