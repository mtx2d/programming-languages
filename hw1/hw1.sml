(*problem 1*)
fun is_older ((y1, m1, d1), (y2, m2, d2)) = 
    if y2 > y1 then true 
    else if m2 > m1 then true 
    else if d2 > d1 then true 
    else false

(*problem 2*)
(* (int * int * int) list * int*)
fun number_in_month (dates, target_month) =
    if dates = [] then 0
    else
        let
            val (_, month, _) = hd dates
            val tl_number_in_month = number_in_month(tl dates, target_month)
        in
            if month = target_month then
                1 + tl_number_in_month
            else
                tl_number_in_month
        end

(*problem 3*)
fun number_in_months (dates, target_months) =
    if target_months = [] orelse dates = [] then 0
    else
        let
            val tm = hd target_months
            val tl_number_in_months = number_in_months(dates, tl target_months)
        in
            number_in_month (dates, tm) + tl_number_in_months
        end


(*problem 4*)
fun dates_in_month (dates, key_month) = 
    if dates = [] then []
    else
        let
            val (year, month, day) = hd dates
            val tl_dates = dates_in_month (tl dates, key_month)
        in
            if month = key_month then
                (year, month, day) :: tl_dates
            else
                tl_dates
        end

(*problem 5*)
fun dates_in_months (dates, months)=
    if months = [] then []
    else
        let
            val key_month = hd months
            val cnt = dates_in_month (dates, key_month)
        in
            cnt @ dates_in_months (dates, tl months)
        end

(*problem 6*)
fun get_nth (ss, n) =
    if n = 1 then hd ss
    else
        get_nth(tl ss, n - 1)

(*problem 7*)
fun date_to_string date = 
    let
        val (y, m, d) = date
        val months = ["January", 
        "February", 
        "March", 
        "April", 
        "May", 
        "June", 
        "July", 
        "August", 
        "September",
        "October",
        "November",
        "December"] 
    in
       get_nth (months, m) ^" "^ (Int.toString d) ^", "^ (Int.toString y)
    end

(*problem 8*)
exception NotEnoughElements
fun number_before_reaching_sum (sum, xs) = 
    if sum <= 0 then ~1 
    else if sum > 0 andalso xs = [] then raise NotEnoughElements
    else
        let
           val cnt = hd xs
        in
           1 + number_before_reaching_sum (sum - cnt, tl xs)
        end

(*problem 9*)
fun what_month day_of_year = 
    let 
       val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
       number_before_reaching_sum (day_of_year, days_in_months) + 1
    end

(*problem 10*)
fun month_range (d1, d2) =
    if d1 > d2 then []
    else
       (what_month d1) :: month_range (d1 + 1, d2)
