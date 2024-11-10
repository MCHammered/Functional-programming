(* Vrne naslednika števila `n`. *)
fun next (n : int) : int = n + 1;

(* Vrne vsoto števil `a` in `b`. *)
fun add (a : int, b : int) : int = a + b;

(* Vrne true, če sta vsaj dva argumenta true, drugače vrne false *)
fun majority (a : bool, b : bool, c : bool) : bool = 
    a andalso b 
      orelse 
    b andalso c
      orelse
    c andalso a;

(* Vrne mediano argumentov - števila tipa real brez (inf : real), (~inf : real), (nan : real) in (~0.0 : real)
   namig: uporabi Real.max in Real.min *)
fun median (a : real, b : real, c : real) : real = 
    Real.max(
            Real.min(a, b), 
            Real.min(Real.max(a, b) , c)
            );

(* Helps a bit *)
fun helpTriangle(first: int, second: int, third: int) : bool = first + second > third;

(* Preveri ali so argumenti veljavne dolžine stranic nekega trikotnika - trikotnik ni izrojen *)
fun triangle (a : int, b : int, c : int)  : bool = 
    helpTriangle(a, b, c) andalso helpTriangle(c, b, a) andalso helpTriangle(a, c, b);


