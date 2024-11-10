(* == KVARTOPIRKSA == *)

datatype barva = Kriz | Pik | Srce | Karo
datatype stopnja = As | Kralj | Kraljica | Fant | Stevilka of int

type karta = stopnja * barva

(* Kakšne barve je karta? *)
fun barvaKarte (k : karta) : barva =
    #2 k;

(* Ali je karta veljavna? *)
fun veljavnaKarta ((Stevilka k,_) : karta) : bool =
        k >= 2 andalso k < 10 |
        veljavnaKarta _ = true;

(* Koliko je vredna karta? *)
fun vrednostKarte (k : karta) : int

(* Kolikšna je vrednost vseh kart v roki? *)  
fun vsotaKart (ks : karta list) : int

(* Ali imam v roki karte iste barve? *)
fun isteBarve (ks : karta list) : bool