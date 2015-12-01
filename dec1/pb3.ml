let score_without w =
    let a = ref (w.[0]) and res = ref 1 in
         for i = 1 to String.length w - 1 do
             if w.[i] <> !a
                 then (incr res; a := w.[i]);
         done;
    !res

let score w =
    let l = String.length w and s = score_without w in
         s + min 2 (l - s)

let main () =
    let w = Scanf.scanf "%d %s" (fun _ w -> w) in
        Printf.printf "%d" (score w);;

main ();;
