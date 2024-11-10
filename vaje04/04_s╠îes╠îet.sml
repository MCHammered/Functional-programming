val squares = List.map (fn x => x * x);
val onlyEven = List.filter (fn x => x mod 2 = 0)
val pf = fn sez => fn f => case f of true => sez | false => sez
(*fun f(x,y) = if String.size x > String.size y then true else false
val bestString =fn sez=> List.foldl(fn (x,y) => if f(x,y) then x else y)*)

(* Vrne skalarni produkt dveh vektorjev. Uporabite List.foldl in ListPair.map. *)
fun dot s1 s2 = List.foldl (fn (x,acc) => x + acc) 0 (ListPair.map(fn (x,y) => x * y) (s1,s2)) 

fun transpose mat = 
    let 
        val conc = List.concat(mat)
    in
    case conc of
    [] => []
    | glava::rep => (List.map hd mat) :: (transpose(List.map tl mat))
    end;

fun multiply mat1 mat2 = 
    List.map(fn v1 => List.map(fn v2 => dot v1 v2) (transpose mat2)) mat1


fun equivalenceClasses f xs =
                        case xs of [] => []
                        | glava :: rep => 
                                          let 
                                             val seznamcek = List.partition(f glava) xs
                                             val eqRazred = #1 seznamcek
                                             val ostanekSez = #2 seznamcek
                                          in
                                              eqRazred :: equivalenceClasses f ostanekSez
                                          end;

fun bestString f xs =
                        case xs of [] => ""
                        | glava :: rep => List.foldl(fn (x,glava) => if f(x,glava) then x else glava) glava xs


fun largestString xs = bestString(fn (x,y) =>if  x > y then true else false) xs
fun longestString xs = bestString(fn (x,y) =>if String.size x > String.size y then true else false) xs


fun turnIntoPairs xs = let 
                          fun insert (elt, []) = [(elt,1)]
                              | insert (elt, ((prestetiElt,stPonovitev)::xs)) =
                                if elt = prestetiElt then (prestetiElt,stPonovitev+1)::xs else (prestetiElt,stPonovitev)::insert(elt, xs)
                        in
                          foldl insert [] xs  
                        end;


fun group [] = []
  | group [edini] = [(edini,1)]
  | group (glava::rep) =
    let
      val pomozna = group(rep) 
    in
      if glava = #1 (hd pomozna)
      then (#1 (hd pomozna), #2 (hd pomozna) + 1)::(tl pomozna)
      else [(glava,1)] @ pomozna
    end;
    
    


fun quicksort f xs =
    case xs of  [] => []
    | glava::rep =>
      let
        val (left, right) = List.partition(fn x => if f(x,glava) = LESS then false else true) xs
      in
        quicksort f left @ [glava] @ quicksort f right
      end;