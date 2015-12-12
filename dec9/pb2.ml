let new_place (x,y) (x0,y0) l =
    let new_xy = ref (x0,y0) in
    (match l with
        | 'R' -> if y0 < y then new_xy := (x0, y0+1)
        | 'L' -> if y0 > 1 then new_xy := (x0, y0-1)
        | 'U' -> if x0 > 1 then new_xy := (x0-1,y0)
        | 'D' -> if x0 < x then new_xy := (x0+1,y0));
    !new_xy

let main () =
    let (lim,pos,w) = Scanf.scanf "%d %d %d %d %s" (fun i j k l m -> ((i,j),(ref (k,l)),m)) and mat = Array.make_matrix 555 555 true and compt = ref 0 in
    for i = 0 to String.length w - 1 do
        let (a,b) = !pos in
            Printf.printf (if mat.(a).(b) = true
                then (mat.(a).(b) <- false; incr compt; "1 ")
                else "0 ");
        pos := new_place lim !pos (w.[i]);
    done;
    Printf.printf "%d" ((fst lim) * (snd lim) - !compt);;

main ();;
