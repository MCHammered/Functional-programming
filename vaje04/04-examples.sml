val swap = fn f => fn x => fn y => fn y x
fun swap f x y = f y x

val compose = fn(f, g) => fn x => f (g x)
fun compose (f, g) x = f (g x)
(* inflix 3 o *)
fun compose (f, g) x = (f o g) x
val compose = op o

val compose2 = fn f => fn g => fn x => f (g x)
fun compose2 f g x = f (g x)

val apply = fn (f, x) => f x
val apply (f, x) = f x
inflixr 2 $
fun f $ x = f x
fun op$ (f, x) = f x
(* f $ g $ h x = f(g(h x)) *)
fun compose (f, g) x = f $ g $ x

fun apply2 = fn f => f x
fun apply2 f x = f x

fun foldl _ acc [] = acc
|   foldl f acc (x :: xs) = foldl f (f (x, acc)) xs
val foldl = List.foldl

fun rev xs = List.fold (fn (x, acc) => x :: acc) [] xs
fun rev xs = List.foldl List.:: [] xs