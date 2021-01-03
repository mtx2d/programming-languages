fun alternate xs: int list = 
        if null xs 
        then NONE
        else 
            if null (tl xs)
            then SOME (hd xs)
            else
                let 
                    val total_length = length xs
                    (* int list -> int * int *)
                    fun alternate_nonempty (xs: int list, len:int) = 
                            if len = 1
                            then ((hd xs), 1)
                            else
                                let 
                                    val tl_ans = alternate_nonempty((tl xs) (len - 1))
                                    val delta = if (len mod 2 = 1) then (hd xs) else (~ (hd xs))
                                in 
                                    (((#1 tl_ans) + delta), ((#2 tl_ans) + 1))
                                end
                in
                    SOME (#1 alternate_nonempty(xs total_length))
                end

fun min_max xs = let fun tl_min_max = min_max (tl xs)
        in NONE
        end