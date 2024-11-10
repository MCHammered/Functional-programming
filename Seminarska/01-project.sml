(* settings for long expressions *)
val _ = Control.Print.printDepth := 100;
val _ = Control.Print.printLength := 1000;
val _ = Control.Print.stringDepth := 1000;
(* disable polyEq warnings *)
val _ = Control.polyEqWarn := false;


(* datatype for logical formulas *)
datatype 'a expression = 
    Not of 'a expression
|   Or of 'a expression list
|   And of 'a expression list
|   Eq of 'a expression list
|   Imp of 'a expression * 'a expression
|   Var of 'a
|   True | False;


(* linear congurence random number generator for function `prTestEq` *)
datatype 'a stream = Next of 'a * (unit -> 'a stream);

fun lcg seed =
    let fun lcg seed =
        Next (seed, fn () =>
            lcg (LargeInt.mod (1103515245 * seed + 12345, 0x7FFFFFFF)))
    in lcg (LargeInt.fromInt seed)
    end;

fun int2bool i = LargeInt.mod (i, 2) = 1;


(* conjutive normal form tester for function `satSolver` *)
fun isCNF (And es) =
    List.all
        (fn Or es => List.all (fn (Var _ | Not (Var _)) => true | _ => false) es
        |   (Var _ | Not (Var _)) => true
        |   _ => false) es
|   isCNF (Or es) = List.all (fn (Var _ | Not (Var _)) => true | _ => false) es
|   isCNF (True | False | Var _ | Not (Var _)) = true
|   isCNF _ = false;
(* exception for function `satSolver` *)
exception InvalidCNF;


(* ==================== SOME HELPER FUN. ==================== *)

(* operator for low priority right associative applications *)
infixr 1 $;
fun f $ x = f x;

(* curried equlity test *)
fun eq a b = a = b;

(* curried inequlity test *)
fun neq a b = a <> b;

(* removes all occurrences of `x` from a list *)
fun remove x = List.filter (neq x);

(* exception for nonimplemented functions *)
exception NotImplemented;


(* ==================== HELPER FUN. ==================== *)

infixr 1 $;
fun f $ x = f x;

fun eq a b = a = b;

fun neq a b = a <> b;

fun remove x = List.filter (neq x);


(* ==================== WARMUP ==================== *)

fun isolate [] = []
  | isolate (glava::rep) = glava::isolate(List.filter (fn x => neq glava x) rep);


(* ==================== PART 1 ==================== *)


fun getVars expr =
    case expr of
        Eq (glava::rep) => isolate ((getVars glava) @ (getVars (Eq rep)))
      | And (glava::rep) => isolate ((getVars glava) @ (getVars (And rep)))
      | Or (glava::rep) => isolate ((getVars glava) @ (getVars (Or rep)))
      | Imp (l,d) => (getVars l) @ (getVars d)
      | Var i => [i]
      | Not i => getVars i
      | _ => [];


fun eval sez expr = 
    case expr of
        Var i => List.exists (fn x => eq x i) sez
      | Not i => not (eval sez i)
      | And i => if null i then true else (eval sez (hd i)) andalso (eval sez (And (tl i)))
      | Or i => if null i then false else (eval sez (hd i)) orelse (eval sez (Or (tl i)))
      | Eq i => if null i then true else eval sez (Or [And i, (And o List.map (fn x => Not x)) i])
      | Imp (l,d) => not (eval sez l) orelse (eval sez d)
      | False => false
      | True => true;


fun rmEmpty expr = 
    case expr of
        Var i => Var i
      | Not i => Not (rmEmpty i)
      | Imp (l,d) => Imp (rmEmpty l, rmEmpty d)
      | And [edinec] => rmEmpty edinec
      | Or [edinec] => rmEmpty edinec
      | Eq [_] => True
      | And [] => True
      | Or [] => False
      | Eq [] => True
      | And sez => And (List.map (fn x => rmEmpty x) sez)
      | Or sez => Or (List.map (fn x => rmEmpty x) sez)
      | Eq sez => Eq (List.map (fn x => rmEmpty x) sez)
      | bool_konst => bool_konst;


fun pushNegations expr =
    let
      fun pomozna expr =
        case expr of
            Var i => Var i
          | Imp (l,d) => rmEmpty (Imp(pomozna l, pomozna d))
          | And i => rmEmpty (And (List.map (fn x => pomozna x) i))
          | Or i => rmEmpty (Or (List.map (fn x => pomozna x) i))
          | Eq i => rmEmpty (Eq (List.map (fn x => pomozna x) i))
          | True => True
          | False => False
          | Not i =>
                case i of
                    Var j => Not (Var j)
                  | Not j => rmEmpty (pomozna j)
                  | Imp (l,d) => rmEmpty (And [pomozna l, (pomozna o Not) d])
                  | And i => rmEmpty (Or (List.map (fn x => (pomozna o Not) x) i))
                  | Or i => rmEmpty (And (List.map (fn x => (pomozna o Not) x) i))
                  | Eq i => rmEmpty (And [Or (List.map (fn x => (pomozna o Not) x) i), Or (List.map (fn x => pomozna x) i)])
                  | bool_konst => Not bool_konst
    in
      pomozna(rmEmpty expr)
    end;

(* ================ POMOZNA FUNKCIJA ================ *)
fun neg True = False
|   neg False = True
|   neg e = Not e;
(* ================================================ *)
fun rmConstants expr = 
  let
    fun pomozna expr =
      case expr of
        Var i => Var i
      | And i =>
        let
          val vezava = (remove True o List.map (fn x => pomozna x)) i
        in
          if List.exists (fn x => eq x False) vezava then False else rmEmpty (And vezava)
        end
      | Or i =>
        let
          val vezava = (remove False o List.map (fn x => pomozna x)) i
        in
          if List.exists (fn x => eq x True) vezava then True else rmEmpty (Or vezava)
        end
      | Eq i =>
        let
          val vezava = List.map (fn x => pomozna x) i
        in
          if List.exists (fn x => eq x True) vezava andalso List.exists (fn x => eq x False) vezava then False
          else if List.exists (fn x => eq x True) vezava then pomozna (And ((List.map (fn x => pomozna x) o remove True) vezava))
          else if List.exists (fn x => eq x False) vezava then pomozna (And ((List.map (fn x => (pomozna o neg) x) o remove False) vezava))
          else rmEmpty (Eq vezava)
        end
      | Imp (l,d) =>
        let
          val vezavaL = pomozna l
          val vezavaD = pomozna d
        in
          if vezavaL = False then True
          else if vezavaL = True then rmEmpty vezavaD
          else if vezavaD = False then rmEmpty (neg vezavaL)
          else if vezavaD = True then True
          else rmEmpty (Imp (vezavaL, vezavaD))
        end
      | Not i => (neg o pomozna) i
      | bool_konst => bool_konst

  in
    pomozna(rmEmpty expr)
  end;

fun rmVars expr =
  let
    fun pomozna expr =
      case expr of
        Var i => Var i
      | Not i => rmEmpty (Not (pomozna i))
      | And i => rmEmpty (And ((isolate o List.map (fn x => pomozna x)) i))
      | Or i => rmEmpty (Or ((isolate o List.map (fn x => pomozna x)) i))
      | Imp (l,d) => 
        let
          val vezavaL = pomozna l
          val vezavaD = pomozna d
        in
          if vezavaL = vezavaD then True else rmEmpty (Imp (vezavaL, vezavaD))
        end
      | Eq i => rmEmpty (Eq ((isolate o List.map (fn x => pomozna x)) i))
      | bool_konst => bool_konst
  in
    pomozna (rmEmpty expr)
  end;


fun simplify expr = 
  let 
    val novi_expr = (rmVars o pushNegations o rmConstants) expr 
  in 
    if expr = novi_expr then expr else simplify novi_expr 
  end;


fun prTestEq seed expr1 expr2 = 
  let      
    fun doloci seed [] = []
      | doloci (Next i) (glava::rep) = if int2bool (#1 i) then glava :: (doloci ((#2 i) ()) rep) else (doloci ((#2 i) ()) rep)

    val spr_expr1 = getVars expr1
    val spr_expr2 = getVars expr2
    val koncni_seznam = doloci (lcg seed) (isolate (spr_expr1 @ spr_expr2))
  in
    eval koncni_seznam expr1 = eval koncni_seznam expr2
  end


(* sat solver *)

fun satSolver _ = raise NotImplemented

(*
fun satSolver (And [] | True) = SOME []
  | satSolver (Or [] | False) = NONE
  | satSolver (And i) = if (not o isCNF) (And i) then raise InvalidCNF else
    let
      fun edinke expr =
        case expr of
          And ((Var i)::r) => i :: edinke (And r)
        | And ((Or[Var i])::r) => i :: edinke (And r)
        | And [] => []
        | And (_::r) => edinke (And r)
        | _ => []
      
      fun rmSingletons expr singl =
        case expr of
          And i => And(List.map (fn x => rmSingletons x singl) i)
        | Or [Var i] => if List.exists (fn x => eq x i) singl then True else Or [Var i]
        | Or i => Or(List.map (fn x => rmSingletons x singl) i)
        | Not (Var i) => if List.exists (fn x => eq x i) singl then False else Not (Var i)
        | Var i => if List.exists (fn x => eq x i) singl then True else Var i
        | c => c

      fun poenostavi expr =
        case expr of
          And i => And((remove True o List.map (fn x => poenostavi x)) i)
        | Or [True] => True
        | Or i => Or((remove False o List.map (fn x => poenostavi x)) i)
        | Not i => (neg o poenostavi) i
        | konst => konst

      fun preveri (And i) = if (List.exists (fn x => eq x Or[]) i) orelse (List.exists (fn x => eq x False) i)s then NONE else SOME []


      val singletoni = edinke (And i)
    in
      SOME []
    end
  | satSolver _ = raise InvalidCNF
*)





  

(*  Za namene terstiranja drugega dela seminarske naloge odkomentiraj
    spodnjo kodo v primeru, da funkcije satSolver nisi implementiral.
    Pred oddajo odstrani nasledji dve vrstici kode!
    Deluje samo za izraze oblike `And [Or [...], Or [...], ....]`*)

(*use "external_sat_solver.sml";
val satSolver = external_sat_solver;*)



(* ==================== PART 2 ==================== *)

type timetable = {day : string, time: int, course: string} list;
type student = {studentID : int, curriculum : string list};


fun problemReduction _ _ _ = raise NotImplemented;

fun solutionRepresentation _ = raise NotImplemented;

