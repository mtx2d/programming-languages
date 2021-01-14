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

fun myfoldr f init xs = 
    case xs of
    [] => init
    | x::xs' => let 
                    val rest = myfoldr f init xs'; 
                in 
                    f x rest
                end

(* ('a -> bool) -> 'a list -> 'a list * 'a list *)
fun partition f xs =
    case xs of
    [] => ([], [])
    | x::xs' => let 
                    val (t_list, f_list) = partition f xs';
                in
                    if f x then (x::t_list, f_list) else (t_list, x::f_list)
                end

(* ('a -> ('b * 'a) option) -> 'a -> 'b list*)
fun unfold f seed =
        case f seed of
        NONE => []
        |SOME(v, new_seed) => v::(unfold f new_seed)
