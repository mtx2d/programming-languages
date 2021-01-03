(*static environment is useful when my program has inconsistent assumptions about variables will not type check*)
(* this is a comment. This is our first program *)
val x = 34;
(* static environment: x : int*)
(* dynamic environment: x --> 34 *)
val y = 17;
(* static environment: x : int, y : int*)
(* dynamic environment: x --> 34, y --> 17 *)
val z = (x + y) + (y + 2);
(* static environment: x : int, y : int, z: int*)
(* typer checker infer type as part of the process and perform this at compile time *)
(* dynamic environment: x --> 34, y --> 17, z --> z *)
val q = z + 1
(* !type checking is done before evaluation*)

val abs_of_z = if z < 0 then 0 - z else z;
(*abs_of_z = int *)
(*if expression does not eval both branches*)

val abs_of_z_simpler = abs z
