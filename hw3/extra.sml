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

fun map2 f (x, y) = (f x, f y)

(* ('a -> 'b list) -> ('c -> 'a list) -> 'c -> 'b list*)
fun app_all f g x = 
    let 
        val lists = List.map f (g x);
        fun merge ls =
            case ls of
            [] => []
            | h::t => h @ (merge t)
    in
        merge lists
    end