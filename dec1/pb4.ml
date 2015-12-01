open Big_int

let yolo = Big_int.big_int_of_int 1000000007;;

let rec pow p x = function
    | 0 -> Big_int.big_int_of_int 1
    | i -> let sq = pow p x (i/2) in
               if i mod 2 = 0
                   then Big_int.mod_big_int (Big_int.mult_big_int sq sq) p
                   else Big_int.mod_big_int (Big_int.mult_big_int (Big_int.mod_big_int (Big_int.mult_big_int sq sq) p) x) p

let rec find tab i =
    if tab.(i) = i
        then i
        else find tab (tab.(i))

let main () =
    let (p,k) = Scanf.scanf "%s %s" (fun i j -> (Big_int.big_int_of_string i,Big_int.big_int_of_string j)) in
    let tab = Array.init (Big_int.int_of_big_int p) (fun i -> i) and tab' = Array.make (Big_int.int_of_big_int p) false and res = ref (-1) in
        for i = 0 to (Big_int.int_of_big_int p)-1 do
             tab.(i) <- find tab (tab.(Big_int.int_of_big_int (Big_int.mod_big_int(Big_int.mult_big_int k (Big_int.big_int_of_int i)) p)));
        done;
        for i = 0 to (Big_int.int_of_big_int p)-1 do
            let a = find tab (tab.(i)) in
                if not (tab'.(a))
                    then (incr res; tab'.(a) <- true);
        done;
        match Big_int.int_of_big_int k with
            | 0 -> Printf.printf "%s" (Big_int.string_of_big_int (pow yolo p ((Big_int.int_of_big_int p) - 1)))
            | 1 -> Printf.printf "%s" (Big_int.string_of_big_int (pow yolo p (Big_int.int_of_big_int p)))
            | _ -> Printf.printf "%s" (Big_int.string_of_big_int (pow yolo p !res));;

main ();;
