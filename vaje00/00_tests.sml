(* 00-HOMEWORK TESTS *)

(* 1. naloga *)
val _ : int -> int = next;
val test11 = next 1 = 2;
val test12 = next 41 = 42;

(* 2. naloga *)
val _ : int * int -> int = add;
val test21 = add(1,2) = 3;
val test22 = add(2,3) = 5;
val test23 = add(~3,3) = 0;
val test24 = add(~4,~5) = ~9;

(* 3. naloga *)
val _ : bool * bool * bool -> bool = majority;
val test31 = majority(false,false,false) = false;
val test32 = majority(false,true,false) = false;
val test33 = majority(true,false,true) = true;
val test34 = majority(true,true,false) = true;
val test35 = majority(true,true,true) = true;
val test36 = majority(false,false,true)=false;

(* 4. naloga *)
val _ : real * real * real -> real = median;
val test41 = Real.==(median(1.0,2.0,3.0),2.0);
val test42 = Real.==(median(~1.0,~3.0,~2.0),~2.0);
val test43 = Real.==(median(5.0,~8.0,10.0),5.0);
val test44 = Real.==(median(~100.0,~42.0,0.0),~42.0);

(* 5. naloga *)
val _ : int * int * int -> bool = triangle;
val test51 = triangle(3,3,3) = true;
val test52 = triangle(1,3,99) = false;