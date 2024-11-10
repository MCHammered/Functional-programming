(* Podan seznam xs agregira z začetno vrednostjo z in funkcijo f v vrednost f (f (f z s_1) s_2) s_3) ... *)
(* Aggregates xs with an initial value z and function f and returns f (f (f z s_1) s_2) s_3) ... *)

fun reduce f z [] = z
    | reduce f z (glava::rep) = f (reduce f z rep) glava;

(* Vrne seznam, ki vsebuje kvadrate števil iz vhodnega seznama. Uporabite List.map. *)
(* Returns a list of squares of the numbers. Use List.map. *)

fun squares [] = []
    | squares sez = List.map(fn x => x*x) sez;

(* Vrne seznam, ki vsebuje vsa soda števila iz vhodnega seznama. Uporabite List.filter. *)
(* Returns a list that contains only even numbers from xs. Use List.filter. *)

fun onlyEven [] = []
    | onlyEven sez = List.filter (fn x => x mod 2 = 0) sez;

(* Vrne najboljši niz glede na funkcijo f (prvi arg.).
 Funkcija f primerja dva niza in vrne true, če je prvi niz boljši od drugega.
  Uporabite List.foldl. Najboljši niz v praznem seznamu je prazen niz. *)

fun bestString f [] = ""
    | bestString f (glava::rep) = List.foldl (fn (x,y) => if f(x,y) then x else y) glava rep;

(* Vrne leksikografsko največji niz. Uporabite bestString. *)

fun largestString sez = bestString (fn (x,y) => x > y) sez;

(* Vrne najdaljši niz. Uporabite bestString. *)

fun longestString sez = bestString (fn (x,y) => String.size x > String.size y) sez;

(* Seznam uredi naraščajoče z algoritmom quicksort. Prvi argument je funkcija za primerjanje. *)

fun quicksort f [] = []
    | quicksort f (glava::rep) =
        let
          val g = (fn x => if f(glava,x) = LESS then false else true);
          val (vecji,manjsi) = List.partition g rep
        in
          (quicksort f vecji) @ [glava] @ (quicksort f manjsi)
        end;

(* Vrne skalarni produkt dveh vektorjev. Uporabite List.foldl in ListPair.map. *)
fun dot sez1 sez2 = 
    List.foldl (fn (x,acc) => x + acc) 0 (ListPair.map (fn (a,b) => a*b) (sez1, sez2));

(* Vrne transponirano matriko. Matrika je podana z vrstičnimi vektorji od zgoraj navzdol:
  [[1,2,3],[4,5,6],[7,8,9]] predstavlja matriko
   [ 1 2 3 ]
   [ 4 5 6 ]
   [ 7 8 9 ]
*)

fun transpose m =
    if null (List.concat m)
	then [] 
    else (List.map hd m) :: (transpose (List.map tl m));
	

(* Zmnoži dve matriki. Uporabite dot in transpose. *)
(* Multiplies two matrices. Use dot and transpose. *)

fun multiply m1 m2 =
    List.map (fn v => List.map (fn s => dot v s) (transpose m2)) m1;


(* V podanem seznamu prešteje zaporedne enake elemente in vrne seznam parov (vrednost, število ponovitev). 
 Podobno deluje UNIX-ovo orodje uniq -c. *)

fun group [] = []
  | group [edini] = [(edini,1)]
  | group (glava::rep) =
    let
      val pomozna = group rep 
    in
      if glava = #1 (hd pomozna)
      then (#1 (hd pomozna), #2 (hd pomozna) + 1)::(tl pomozna)
      else [(glava,1)] @ pomozna
    end;

(* Elemente iz podanega seznama razvrsti v ekvivalenčne razrede. 
Znotraj razredov naj bodo elementi v istem vrstnem redu kot v podanem seznamu. 
Ekvivalentnost elementov definira funkcija f, ki za dva elementa vrne true, če sta ekvivalentna. *)

fun equivalenceClasses f [] = []
    | equivalenceClasses f sez =
      let 
        val (razred, ostanek) = List.partition (f (hd sez)) sez
      in
        razred :: equivalenceClasses f ostanek
      end;