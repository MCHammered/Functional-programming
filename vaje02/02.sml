datatype number = Zero | Succ of number | Pred of number;

(* Sestavite funkcijo simp : number -> number,
 ki dano predstavitev pretvori v najbolj ekonomično,
 se pravi tako, ki ima najmanjše možno število konstruktorjev. *)

 fun simp Zero = Zero
    | simp (Succ i) = 
        (case simp i of
            Pred j => j
            |    j => Succ j)
    | simp (Pred i) =
        (case simp i of
            Succ j => j
           |     j => Pred j);




(* Negira število a. Pretvorba v int ni dovoljena! *)
fun neg (a : number) : number =
    case a of
        Zero => Zero
      | Succ k => Pred(neg k)
      | Pred k => Succ(neg k);

(* Vrne vsoto števil a in b. Pretvorba v int ni dovoljena! *)
fun add (a : number, b : number) : number =
    case a of
        Succ i => Succ(add(i,b))
       |Pred i => Pred(add(i,b))
       |Zero => b;



(* Vrne rezultat primerjave števil a in b. Pretvorba v int ter uporaba funkcij `add` in `neg` ni dovoljena!
    namig: uporabi funkcijo simp *)
fun comp (a : number, b : number) : order =
    case simp(add(a, neg b)) of
        Zero => EQUAL
      | Succ _ => GREATER
      | Pred _ => LESS;




datatype tree = Node of int * tree * tree | Leaf of int;

(* Vrne true, če drevo vsebuje element x. *)
fun contains (tree : tree, x : int) : bool =
    case tree of
        Leaf i => i = x
      | Node (i,l,d) => i = x orelse contains(l,x) orelse contains(d,x);  

(* Vrne število listov v drevesu. *)
fun countLeaves (tree : tree) : int =
    case tree of
        Leaf _ => 1
      | Node (_,l,d) => countLeaves l + countLeaves d;


(* Vrne število število vej v drevesu. *)
fun countBranches (tree : tree) : int =
    case tree of
        Leaf _ => 0
      | Node (_,l,d) => 2 + countBranches l + countBranches d;


(* Vrne višino drevesa. Višina lista je 1. *)
fun height (tree : tree) : int =
    case tree of
        Leaf _ => 1
      | Node (_,l,d) => 1 + Int.max(height l,height d);


(* Pretvori drevo v seznam z vmesnim prehodom (in-order traversal). *)
fun toList (tree : tree) : int list =
    case tree of
        Leaf i => i::nil
      | Node (i,l,d) =>
            let
                fun zdruzi_sez(sez1: int list, sez2: int list) : int list =
                    if null sez1
                    then sez2
                    else (hd sez1)::zdruzi_sez(tl sez1,sez2)
            in
                zdruzi_sez(toList l,i::toList d)
            end;



(* Vrne true, če je drevo uravnoteženo: 
 * - Obe poddrevesi sta uravnoteženi.
 * - Višini poddreves se razlikujeta kvečjemu za 1.
 * - Listi so uravnoteženi po definiciji.
 *)
 
fun isBalanced (tree : tree) : bool =
    case tree of
        Leaf i => true
      | Node (_,l,d) => isBalanced l andalso isBalanced d andalso abs(height l - height d) <= 1;


(*
 Vrne true, če je drevo binarno iskalno drevo:
 * - Vrednosti levega poddrevesa so strogo manjši od vrednosti vozlišča.
 * - Vrednosti desnega poddrevesa so strogo večji od vrednosti vozlišča.
 * - Obe poddrevesi sta binarni iskalni drevesi.
 * - Listi so binarna iskalna drevesa po definiciji.
 *)

fun isBST (tree : tree) : bool =
    case tree of
        Leaf i => true
      | Node (i,l,d) =>
            let
                fun preveri_traversal(seznam : int list) : bool =
                     if null(tl seznam)
                     then true
                     else
                        hd seznam < hd(tl seznam) andalso preveri_traversal(tl seznam)
            in
                preveri_traversal(toList(tree))
            end;






