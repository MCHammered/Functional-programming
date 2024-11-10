
(*
V programskem jeziku SML implementirajte naslednji modul (programski vmesnik)
 za delo z racionalnimi števili. Vaša struktura naj se imenuje Rational
 in naj definira podatkovni tip (datatype) rational 
 (ki je lahko bodisi celo število bodisi ulomek).

Ulomki naj sledijo naslednjim pravilom:
 Imenovalec je vedno strogo večji od 1.
 Vsak ulomek je okrajšan.
*)

signature RATIONAL =
sig
    (* Definirajte podatkovni tip rational, ki podpira preverjanje enakosti. *)
    eqtype rational

    (* Definirajte izjemo, ki se kliče pri delu z neveljavnimi ulomki - deljenju z nič. *)
    exception BadRational

    (* Vrne racionalno število, ki je rezultat deljenja dveh podanih celih števil. *)
    val makeRational: int * int -> rational

    (* Vrne nasprotno vrednost podanega števila. *)
    val neg: rational -> rational

    (* Vrne obratno vrednost podanega števila. *)
    val inv: rational -> rational

    (* Funkcije za seštevanje in množenje. Rezultat vsake operacije naj sledi postavljenim pravilom. *)
    val add: rational * rational -> rational
    val mul: rational * rational -> rational

    (* Vrne niz, ki ustreza podanemu številu.
        Če je število celo, naj vrne niz oblike "x" oz. "~x".
        Če je število ulomek, naj vrne niz oblike "x/y" oz. "~x/y". *)
    val toString: rational -> string
end

signature RATIONAL =
sig
    (* Definirajte podatkovni tip rational, ki podpira preverjanje enakosti. *)
    eqtype rational

    (* Definirajte izjemo, ki se kliče pri delu z neveljavnimi ulomki - deljenju z nič. *)
    exception BadRational

    (* Vrne racionalno število, ki je rezultat deljenja dveh podanih celih števil. *)
    val makeRational: int * int -> rational

    (* Vrne nasprotno vrednost podanega števila. *)
    val neg: rational -> rational

    (* Vrne obratno vrednost podanega števila. *)
    val inv: rational -> rational

    (* Funkcije za seštevanje in množenje. Rezultat vsake operacije naj sledi postavljenim pravilom. *)
    val add: rational * rational -> rational
    val mul: rational * rational -> rational

    (* Vrne niz, ki ustreza podanemu številu.
        Če je število celo, naj vrne niz oblike "x" oz. "~x".
        Če je število ulomek, naj vrne niz oblike "x/y" oz. "~x/y". *)
    val toString: rational -> string
end


structure Rational :> RATIONAL =
struct

    type rational = int * int
    exception BadRational

    fun rational (a, b) =
        if b = 0 then raise BadRational
        else 
            let
                fun gcd a b =
                    if a = 0
                    then b
                    else gcd (b mod a) a
                
                val naj_del = abs (gcd a b)
            in
                (a div naj_del, b div naj_del )
            end

    fun makeRational (a, b) = if b = 0 then raise BadRational
        else if a*b < 0 then rational (~ (abs a), abs b)
        else rational (abs a, abs b)
    
    fun neg ((a, b): rational) = let open Int in (~a, b) end

    fun inv ((a, b): rational) = if a = 0 then raise BadRational else makeRational (b, a)

    fun add (((a, b), (c, d)): rational * rational) = let open Int in rational (a*d + c*b, b*d) end

    fun mul (((a, b), (c, d)): rational * rational) = let open Int in rational (a*c, b*d) end

    fun toString ((a, 1): rational) = let open Int in Int.toString a end
    | toString ((a, b): rational) = let open Int in Int.toString a ^ "/" ^ Int.toString b end
end;

(*
Implementirajte tudi funktor SetFn,
 ki ustreza podpisu SETFN. Funktor vrača modul,
 ki je podpisan s podpisom SET,
 ki ima razkrito definicijo podatkovnega tipa type.
*)

signature EQ =
sig
    type t
    val eq : t -> t -> bool
end

signature SET =
sig
    (* podatkovni tip za elemente množice *)
    type item

    (* podatkovni tip množico *)
    type set

    (* prazna množica *)
    val empty : set

    (* vrne množico s samo podanim elementom *)
    val singleton : item -> set

    (* unija množic *)
    val union : set -> set -> set

    (* razlika množic (prva - druga) *)
    val difference : set -> set -> set

    (* a je prva množica podmnožica druge *)
    val subset : set -> set -> bool
end

(*funsig SETFN (Eq : EQ) = SET *)


functor SetFn (Eq : EQ) : SET where type item = Eq.t =
struct
    
    type item = Eq.t
    type set = item list ref

    val empty = ref [] : set

    fun singleton (a : item) = ref [a]
    fun union a b =
        let
          fun zdruzi a b =
            case a of
                (g::r) =>  g :: (zdruzi r b)
              | [] => b
          
          fun isolate [] = []
            | isolate (glava::rep) = glava::isolate(List.filter (fn x => not (Eq.eq glava x)) rep)
        in
          ref (isolate (zdruzi (!a) (!b)))
        end
    fun difference a b =
        let
          fun razlika [] b = []
          | razlika a [] = a
          | razlika (g::r) b = if List.exists (fn x => Eq.eq x g) b then razlika r b else g :: razlika r b
        in
          ref (razlika (!a) (!b))
        end
    fun subset a b =
        let
          fun podmnozica [] _ = true
          | podmnozica _ [] = false
          | podmnozica (g::r) b = List.exists (fn x => Eq.eq x g) b andalso podmnozica r b
        in
          podmnozica (!a) (!b)
        end
        
end;

