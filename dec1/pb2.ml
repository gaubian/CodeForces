let main () =
   let (n,k) = Scanf.scanf "%d %d" (fun i j -> (i,j)) in
   let tab = Array.make (2 * k) 0 in
       for i=1 to n do
           Scanf.scanf " %d" (fun j -> tab.(n - i) <- j);
       done;
   let res = ref 0 in
       for i=0 to 2*k - 1 do
           res := max !res (tab.(i) + tab.(2 * k - i - 1));
       done;
   Printf.printf "%d" !res;;

main ();;
