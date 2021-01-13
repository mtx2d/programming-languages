fun compose_opt f g x =
    case g x of
        NONE => NONE
        | SOME(v) => f v

fun do_until f p x =
    case p x of
        false => x
        |true => do_until f p (f x)

fun fact n = let 
                 val (ans, _) = do_until (fn (prod, cnt) => (prod * cnt, cnt + 1)) (fn (prod, cnt) => cnt <> (n + 1)) (1, 1)
             in
                 ans
             end

fun fixed_point f x = 
    do_until f (fn x => (f x) = x) x