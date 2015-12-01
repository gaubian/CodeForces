let score m' w' x' =
    let m = float m' and w = float w' and x = float x' in
 max (0.3 *. x) ((1. -. m /. 250.) *. x -. 50. *. w)

let main () =
    let (m1,m2,m3,m4,m5,w1,w2,w3,w4,w5,hs,hu) = Scanf.scanf
    "%d %d %d %d %d %d %d %d %d %d %d %d"
    (fun a b c d e f g h i j k l -> (a,b,c,d,e,f,g,h,i,j,k,l)) in
         let sc = score m1 w1 500 +. score m2 w2 1000 +. score m3 w3 1500 +. score m4 w4 2000 +. score m5 w5 2500 +. 100. *. (float hs) -. 50. *. (float hu) in
             Printf.printf "%d" (int_of_float sc);;

main ();;
