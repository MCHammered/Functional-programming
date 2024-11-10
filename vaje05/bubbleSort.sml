fun swap (tab,i,j) =
    let
      val el = Array.sub (tab, i)
    in
      Array.update (tab, i, Array.sub (tab, j));
      Array.update (tab, j, el)
    end

fun bubbleSort sez =
    let
      val n = Array.length sez
      val i = ref 0
      val j = ref 0
    in
      while !i < n - 1 do(
        j := 0;
        while !j < n - 1 - !i do(
            if Array.sub (sez,!j) > Array.sub (sez,!j + 1)
            then (swap (sez, !j,!j + 1) ; j := !j + 1)
            else j := !j + 1
            )
       )

    end