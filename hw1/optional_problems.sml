fun alternate xs = 
        if null xs 
        then NONE
        else 
            if null (tl xs)
            then hd xs
            else
                let 
                    val len = length (tl xs)
                    (* int list -> int * int *)
                    fun alternate_nonempty (xs len) = 
                            if len = 1
                            then ((hd xs) 0)
                            else
                                let val tl_ans= alternatve_nonempty (tl xs len - 1)
                                in ((#1 tl_ans) (#2 tl_ans))
                                end
                in
                    SOME alternatve_nonempty ((tl xs) len)
                end

fun min_max xs = let fun tl_min_max = min_max (tl xs)
        in NONE
        end