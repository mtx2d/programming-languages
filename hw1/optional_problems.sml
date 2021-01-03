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

fun min_max xs = if null xs then (NONE, NONE) else let
                val tl_res = min_max (tl xs)
                val tl_min = if isSome (#1 tl_res) then (#1 tl_res) else SOME(hd xs)
                val tl_max = if isSome (#2 tl_res) then (#2 tl_res) else SOME(hd xs)
            in 
                (if not (isSome tl_min) orelse hd xs < (valOf tl_min) then SOME(hd xs) else tl_min, 
                    if not (isSome tl_max) orelse hd xs > (valOf tl_max) then SOME(hd xs) else tl_max)
            end


fun repeat (vs, ns) = 
        if null vs andalso null ns then
            []
        else
            let 
                val tl_ans = repeat(tl vs, tl ns)
                fun populate (v, n) =
                        if n = 0 then [] else [v] @ populate(v, n - 1)
            in
                populate(hd vs, hd ns) @ tl_ans
            end