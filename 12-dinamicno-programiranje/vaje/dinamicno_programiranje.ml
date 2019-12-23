(* ========== Vaje 6: Dinamično programiranje  ========== *)


(*----------------------------------------------------------------------------*]
 Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
 samo za eno polje navzdol ali za eno polje na desno in na koncu mora prispeti
 v desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
 različne (ne-negativne) mase. Miška bi se rada kar se da nažrla, zato jo
 zanima, katero pot naj ubere.

 Funkcija [max_cheese cheese_matrix], ki dobi matriko [cheese_matrix] z masami
 sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
 optimalni poti.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_cheese test_matrix;;
 - : int = 13
[*----------------------------------------------------------------------------*)

let test_matrix = 
  [| [| 1 ; 2 ; 0 |];
     [| 2 ; 4 ; 5 |];
     [| 7 ; 0 ; 1 |] |]

let max_cheese cheese_matrix = 
  let max_i = Array.length cheese_matrix in
  let max_j = Array.length cheese_matrix.(0) in 
  let max_matrix = Array.make_matrix max_i max_j 0 in 
  let how_much cheese i j  =
    let cheese = cheese_matrix.(i).(j) in 
    if i < (max_i-1) then 
      if j < (max_j-1) then cheese + max ( max_matrix.(i+1).(j)  max_martix(i).(j+1) )
    else cheese + max_matrix.(i+1).(j)
    else if j < (max_j-1) then cheese + max_martix(i).(j+1) 
    else cheese
  in
  let how_much_cheese2 i j = 
    let cheese = cheese_matrix.(i).(j) in 
    let max_right = if j < (max_j -1) then max_matrix.(i).(j+1) else 0 in 
    let max_down = if i < (max_i -1) then max_matrix.(i+1).(j) else 0 in
      cheese+ max max_right max_down
  in 
  let rec loop i j = 
  let cheese = how_much_cheese i j in 
  let () = max_matrix.(i).(j) <- cheese in 
    if j>0 then 
      (* vse je ok  *)
      loop i (j-1)
    else
        (* moramo skocit v novo vrstico   *)
     	if i>0  then loop (i-1) (max_j-1) 
      else ()

  in 
  let () = loop (max_i-1) (max_j-1) in max_matrix
        


(*----------------------------------------------------------------------------*]
 Rešujemo problem sestavljanja alternirajoče obarvanih stolpov. Imamo štiri
 različne tipe gradnikov, dva modra in dva rdeča. Modri gradniki so višin 2 in
 3, rdeči pa višin 1 in 2.

 Funkcija [alternating_towers] za podano višino vrne število različnih stolpov
 dane višine, ki jih lahko zgradimo z našimi gradniki, kjer se barva gradnikov
 v stolpu izmenjuje (rdeč na modrem, moder na rdečem itd.). Začnemo z gradnikom
 poljubne barve.

 Namig: Uporabi medsebojno rekurzivni pomožni funkciji z ukazom [and].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # alternating_towers 10;;
 - : int = 35
[*----------------------------------------------------------------------------*)
type color  = 
|Red 
|Blue
        (* 1.establish bounds
           2. make memory 
           3. calculate one value by using recursion with memory 
           4. loop oveer all values in the correct order
           5. return result *)
let alternating_towers h = 
  let red_mem = Array.make (h+1) 0 in 
  let blue_mem = Array.make (h+1) 0 in 
  let red_bottom =
    match h with 
    |0 -> 0 
    |1|2 -> 1
    |h -> blue_mem.(h-1) + blue_mem.(h-2)
  in 
  let blue_bottom  = function
 
    |0|1 -> 0
    |2-> 1
    |3-> 2
    |h -> red_mem.(h-2)+red_mem(h-3)
  in 
  let rec loop n =
    if n > h then () else 
    let _ = red_mem.(n) <- red_bottom n in 
    let _ = blue_mem.(n) <- blue_bottom n in 
    loop (n+1)  
  in 
  let _ = loop 0 in  red_mem.(h)+blue_mem.(h)
  








(*----------------------------------------------------------------------------*]
 Na nagradni igri ste zadeli kupon, ki vam omogoča, da v Mercatorju kupite
 poljubne izdelke, katerih skupna masa ne presega [max_w] kilogramov. Napišite
 funkcijo [best_value articles max_w], ki poišče največjo skupno ceno, ki jo
 lahko odnesemo iz trgovine, kjer lahko vsak izdelek vzamemo večkrat, nato pa
 še funkcijo [best_value_uniques articles max_w], kjer lahko vsak izdelek
 vzamemo kvečjemu enkrat.

 Namig: Modul [Array] ponuja funkcije kot so [map], [fold_left], [copy] in
 podobno, kot alternativa uporabi zank.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # best_value articles 1.;;
 - : float = 10.95
 # best_value_unique articles 1.;;
- : float = 7.66
[*----------------------------------------------------------------------------*)

(* Articles are of form (name, price, weight) *)
let articles = [|
	("yoghurt", 0.39, 0.18);
	("milk", 0.89, 1.03);
  ("coffee", 2.19, 0.2);
  ("butter", 1.49, 0.25);
  ("yeast", 0.22, 0.042);
  ("eggs", 2.39, 0.69);
  ("sausage", 3.76, 0.50);
  ("bread", 2.99, 1.0);
  ("Nutella", 4.99, 0.75);
  ("juice", 1.15, 2.0)
|]


(*----------------------------------------------------------------------------*]
 Cena sprehoda po drevesu je vsota vrednosti v vseh obiskanih vozliščih.
 Poiščite vrednost najdražjega sprehoda od korena do listov drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_path Empty ;;
 - : 'a option = None
 # max_path test_tree;;
- : int option = Some 21
[*----------------------------------------------------------------------------*)

type 'a tree
 = Empty
 | Node of ('a tree) * 'a * ('a tree)

let leaf x = Node (Empty, x, Empty)

let test_tree = Node( Node(leaf 0, 2, leaf 13), 5, Node(leaf 9, 7, leaf 4))

(*----------------------------------------------------------------------------*]
 Cena sprehoda po drevesu je vsota vrednosti v vseh obiskanih vozliščih.
 Poiščite najdražji sprehod od korena do listov drevesa: Funkcija pot vrne v 
 obliki seznama smeri, katere je potrebno izbrati za najdražji sprehod.

 Napišite tudi funkcijo, ki sprehod pretvori v elemente sprehoda
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_path_trace Empty ;;
 - : 'a list = []
 # max_path_trace test_tree;;
- : direction list = [Right, Left]
 # reconstruct test_tree (max_path_trace test_tree);;
- : int list = [5; 7; 9]
[*----------------------------------------------------------------------------*)

type direcion 
  = Left
  | Right