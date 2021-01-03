fun alternate (xs: int list) = 
        if null xs 
        then NONE
        else 
            if null (tl xs)
            then SOME (hd xs)
            else
                let 
                    (* int list -> int * int *)
                    fun alternate_nonempty (xs: int list, idx:int) = 
                            if null xs
                            then (0, idx)
                            else
                                let 
                                    val tl_ans = alternate_nonempty(tl xs, idx + 1)
                                    val delta = if (idx mod 2 = 0) then (hd xs) else (~ (hd xs))
                                in 
                                    ((#1 tl_ans) + delta, idx)
                                end
                    val res = alternate_nonempty(xs, 0)
                in
                    SOME (#1 res)
                end
