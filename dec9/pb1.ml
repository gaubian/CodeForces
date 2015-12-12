let main () =
    let (a,b,c,x,y,z) = Scanf.scanf "%d %d %d %d %d %d" (fun i j k l m n -> (i,j,k,l,m,n)) in
    let a' = a - x and b' = b - y and c' = c - z in
    let too_much = List.fold_left (+) 0 (List.map (fun i -> if i > 0 then i / 2 else i) [a';b';c']) in
        print_endline
        (if too_much >= 0 then "Yes" else "No");;

main ();;
 
