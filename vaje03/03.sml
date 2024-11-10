datatype natural = Succ of natural | One;
exception NotNaturalNumber;

datatype 'a bstree = br of 'a bstree * 'a * 'a bstree | lf;
datatype direction = L | R;

(* zip (x, y): Vrne seznam, ki ima na i-tem mestu par (x_i, y_i),
 v kateri je x_i i-ti element seznama x, y_i pa i-ti element seznama y.
  Če sta dolžini seznamov različni, vrnite pare do dolžine krajšega. *)
fun zip(sez1 : 'a list, sez2 : 'b list) : ('a  * 'b) list =
    let
        fun pomozna(sezn1, sezn2, aux) : ('a  * 'b) list =
            if null sezn1 orelse null sezn2
            then aux
            else pomozna(tl sezn1, tl sezn2, aux@[(hd sezn1, hd sezn2)])
    in
        pomozna(sez1,sez2,[])
    end;


(* "Pseudoinverz" funkcije zip *)
fun unzip(seznam : ('a * 'b) list) : 'a list * 'b list  =
    let
        fun pomozna(sez : ('a * 'b) list, aux1, aux2) : 'a list * 'b list =
            if null sez
            then (aux1,aux2)
            else pomozna(tl sez, aux1@[#1 (hd sez)],aux2@[#2 (hd sez)])
    in
        pomozna(seznam,[],[])
    end;

(* subtract (a, b): Vrne naravno število, ki ustreza razliki števil
 a in b(a - b). Če rezultat ni naravno število, proži izjemo NotNaturalNumber. *)

fun subtract(a : natural, b : natural) : natural =
    let
        fun pretvori_v_int(x : natural) : int =
            case x of
                One => 1
              | Succ i => 1 + pretvori_v_int i
        
        fun pretvori_v_natural(numb : int) : natural =
            if numb=1
            then One
            else Succ(pretvori_v_natural(numb-1))

        val x1 = pretvori_v_int a 
        val x2 = pretvori_v_int b
        
    in
        if (x1 - x2) <= 0
        then raise NotNaturalNumber
        else pretvori_v_natural(x1 - x2)
    end;
    

(* any (f, s): Vrne true, če funkcija f vrne true za kateri koli element seznama s.
 Za prazen seznam naj vrne false *)
fun any(f, s : 'a list) : bool =
    case s of
        [] => false
      | glava::rep => f(glava) orelse any(f, rep);

(* map (f, s): Vrne seznam elementov, preslikanih s funkcijo f vhodnega seznama s. *)
fun map(f, s : 'a list) : 'b list =
    case s of
        [] => []
      | glava::rep => (f glava)::map(f,rep);

(* filter (f, s): Vrne seznam elementov, za katere funkcija f vrne true *)
fun filter(f, s : 'a list) : 'a list =
    case s of
        [] => []
      | glava::rep => if (f glava)
                      then glava::filter(f, rep)
                      else filter(f, rep);

(* fold (f, z, s): Izračuna in vrne f(\dots f(f(z, s_1), s_2),\dots s_n) *)
fun fold(f, z : 'a, s : 'b list) : 'a =
    case s of
        [] => z
      | glava::rep => fold(f, f(z,glava), rep);




    


    