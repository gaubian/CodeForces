let main () =
    let n = Scanf.scanf "%d" (fun i -> i) in
    let tab = Array.make n 0 and l = ref [] and liste = ref [] in
    let rec aux = function
        | [_,b]   -> b,n
        | (_,c)::q -> let (b,last) = aux q in
                      if c > b
                          then (c,last)
                          else (tab.(c) <- last - c;last + 1,last + 1)
    in
        for i = 0 to n - 1 do
            Scanf.scanf " %d" (fun j -> l := (j - 1,i)::!l; liste := (j - 1,i)::!liste);
        done;
    let l' = List.sort (fun i j -> - compare i j) !l in
    let (a,b) = aux l' in
    let l = ref [] and hasht = Hashtbl.create 0 in
        Array.iter (fun i -> try
        (let j = Hashtbl.find hasht i in Hashtbl.replace hasht i (j+1))
         with _ -> (l := i::!l;Hashtbl.add hasht i 1)) tab;
    let first_sol = (n - List.fold_left max 0 (List.map (Hashtbl.find hasht) !l)) in
        Printf.printf "%d" (first_sol);
    let i_m = List.assoc 0 !liste in
    let liste' = List.map (fun (i,j) -> if i = i_m then (0,j) else if i < i_m then (i+1,j) else (i,j)) !liste in
    let tab = Array.make n 0 and l' = List.sort (fun i j -> - compare i j) liste' in
    let (a,b) = aux l' in
    let l = ref [] and hasht = Hashtbl.create 0 in
        Array.iter (fun i -> try
        (let j = Hashtbl.find hasht i in Hashtbl.replace hasht i (j+1))
         with _ -> (l := i::!l;Hashtbl.add hasht i 1)) tab;
    let second_sol = 1 + (n - List.fold_left max 0 (List.map (Hashtbl.find hasht) !l)) in
        Printf.printf "%d" (min first_sol second_sol);;

main ();;
