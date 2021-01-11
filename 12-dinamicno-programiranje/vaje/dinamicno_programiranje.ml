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
 Poleg količine sira, ki jo miška lahko poje, jo zanima tudi točna pot, ki naj
 jo ubere, da bo prišla do ustrezne pojedine.

 Funkcija [optimal_path] naj vrne optimalno pot, ki jo mora miška ubrati, da se
 čim bolj nažre. Ker je takih poti lahko več, lahko funkcija vrne poljubno.
 Pripravite tudi funkcijo [convert_path], ki pot pretvori v seznam tež sirčkov
 na poti.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # optimal_path_bottom test_matrix;;
 - : mouse_direction list = [Right; Down; Down; Right; Down]
 # optimal_path test_matrix |> convert_path test_matrix;;
 - : int list = [1; 2; 4; 5; 1]
[*----------------------------------------------------------------------------*)

type mouse_direction = Down | Right


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
 Izračunali smo število stolpov, a naše vrle gradbince sedaj zanima točna
 konfiguracija. Da ne pride do napak pri sestavljanju, bomo stolpe predstavili
 kar kot vsotne tipe. 

 Stolp posamezne barve so temelji (Bottom), ali pa kot glava bloka pripadajoče
 barve in preostanek, ki je stolp nasprotne barve.

 Definirajte funkcijo [enumerate_towers], ki vrne seznam vseh stolpov podane
 dolžine. Stolpe lahko vrne v poljubnem vrstnem redu. Funkcija naj hitro (in
 brez) prekoračitve sklada deluje vsaj do višine 20.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # enumerate_towers 4;;
 - : tower list = 
    [Red (TopRed (Red2, TopBlue (Blue2, RedBottom)));
     Red (TopRed (Red1, TopBlue (Blue3, RedBottom)));
     Red (TopRed (Red1, TopBlue (Blue2, TopRed (Red1, BlueBottom))));
     Blue (TopBlue (Blue3, TopRed (Red1, BlueBottom)));
     Blue (TopBlue (Blue2, TopRed (Red2, BlueBottom)))]
[*----------------------------------------------------------------------------*)


type blue_block = Blue3 | Blue2
type red_block = Red2 | Red1

type red_tower = TopRed of red_block * blue_tower | RedBottom
and blue_tower = TopBlue of blue_block * red_tower | BlueBottom

type tower = Red of red_tower | Blue of blue_tower

(*----------------------------------------------------------------------------*]
 Vdrli ste v tovarno čokolade in sedaj stojite pred stalažo kjer so ena ob
 drugi naložene najboljše slaščice. Želite si pojesti čim več sladkorja, a
 hkrati poskrbeti, da vas ob pregledu tovarne ne odkrijejo. Da vas pri rednem
 pregledu ne odkrijejo, mora biti razdalija med dvema zaporednima slaščicama,
 ki ju pojeste vsaj `k`.

 Napišite funkcijo [ham_ham], ki sprejme seznam naravnih števil dolžine `n`, ki
 predstavljajo količino sladkorja v slaščicah v stalaži in parameter `k`,
 najmanjšo razdalijo med dvema slaščicama, ki ju še lahko varno pojeste.
 Funkcija naj vrne seznam zastavic `bool`, kjer je `i`-ti prižgan natanko tedaj
 ko v optimalni požrtiji pojemo `i`-to slaščico.

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # ham_ham test_shelf 1;;
 - : bool list = [false; true; false; true; false; true; false; true; false]
 # ham_ham test_shelf 2;;
 - : bool list = [false; true; false; false; false; true; false; false; false]
[*----------------------------------------------------------------------------*)

let test_shelf = [1;2;-5;3;7;19;-30;1;0]
