(* ========== Vaja 2: Funkcijsko Programiranje  ========== *)

(*----------------------------------------------------------------------------*]
Namig: Definirajte pomožno funkcijo za obračanje seznamov.
[*----------------------------------------------------------------------------*)

(*lepljenje seznamov je zelo časovno zamudno*)
let rec bad_reverse  = function
  |x::xs -> bad_reverse xs @ [x]   (*časovno zelo slaba izbira*)
    |[]->[]


 (*sezname delaš z match with !!!!... šele ko nardis
 lahko zamenjaš na obliko function  in s tem polepšaš*)
 (*na tak način pravilno obračas sezname. hitreje*)
let reverse list =
  let rec reverse_aux acc (*list=
   match list with*)=function
   | [] -> acc
   | x :: xs -> reverse_aux (x :: acc) xs
  in 
  reverse_aux [] list 
 (*učasih se zgodi da se  funkcija zaklene za določen tip in potem neče delat na drugih tipih.*)
(*kasneje se nam je sesu na funkciji zip ker je accumulator imeu notr pare, 
medtem ko smo ga prej poklicali na nečem kar niso pari, ( v prejšnjih nalogah))
ta dva tipa nista združljiva . 
v range n smo reversali , in se je zaklenu na integerje *)




(*----------------------------------------------------------------------------*]
 Funkcija [repeat x n] vrne seznam [n] ponovitev vrednosti [x]. Za neprimerne
 vrednosti [n] funkcija vrne prazen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # repeat "A" 5;;
 - : string list = ["A"; "A"; "A"; "A"; "A"]
 # repeat "A" (-2);;
 - : string list = []
[*----------------------------------------------------------------------------*)
(*nesmisln match with ker imamo naravna števila. IF STAVEK JE BOLJŠI*)
let rec repeat x n =  
  if  n<=0 then []
  else x :: repeat x (n-1)


 (*repna rekurzija*)
let rec repeat_tlrec x n =
  let rec aux  (*x*)n acc= 
  (*v pomožni funk. x ni potreben kot argument*)
    if n<=0 then acc
    else aux (*x*)(n-1) (x::acc)
  in aux (*x*)n []
  

(*----------------------------------------------------------------------------*]
 Funkcija [range] sprejme število in vrne seznam vseh celih števil od 0 do
 vključno danega števila. Za neprimerne argumente funkcija vrne prazen seznam.
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # range 10;;
 - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
[*----------------------------------------------------------------------------*)

(* TALE NI REPNA *) (* lepljenje seznamov je zamudno. *)
let (*rec*) range_not_tailrec n = 
  let rec range_from m = 
    if m>n then [] else m::(range_from(m+1))
  in range_from 0
(* razlaga :  preveri m in izberi pot. če je m<=n naredi seznam, tako da m prilepis na ostanek seznama,
na katerega kličeš rekurzijo. seznam se nenehno posodablja, in ni sestavljen do zadnjega koraka.. ZELO ZAMUDNO *)
let rec range_slow n = 
  if n<=0 then [] else (range_slow (n-1)) @[n]


(* TALE JE REPNA *)
  let range_tlrec n = 
    let rec range_aux n acc = 
      if n<0 then acc else range_aux(n-1)(n::acc)
    in range_aux n []
(*razlaga: acc=[] , n =5 
1.korak. vzames 5 in jo dodaš v accumulator : 5::[], n-1
2.korak  vzameš n=4 in dodaš v accumulator : 4::[5], n-1
... torej ! .... rekurzija s korakom (n-1) (n::acc) *)


(*----------------------------------------------------------------------------*]
 Funkcija [map f list] sprejme seznam [list] oblike [x0; x1; x2; ...] in
 funkcijo [f] ter vrne seznam preslikanih vrednosti, torej
 [f x0; f x1; f x2; ...].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (+) 2 in
   map plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let rec map f (*list = match list with *)= function  
| [] -> []
| x::xs -> f x ::(map f xs)


(*----------------------------------------------------------------------------*]
 Funkcija [map_tlrec] je repno rekurzivna različica funkcije [map].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (fun x -> x + 2) in
   map_tlrec plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let rec map_tlrec f list =
  (*f se ne spremeni, zto ni potreben kot argument*)
  let rec map_aux list acc = 
    match list with 
    |[]-> reverse acc (* seznam se sestavi v nasprostnni smeri, zato ga na skoncu obrnemo*)
    |x::xs -> map_aux xs ( f x ::acc)
  in map_aux list [] 
  

(*----------------------------------------------------------------------------*]
 Funkcija [mapi] je ekvivalentna python kodi:

  def mapi(f, list):
      mapi_list = []
      i = 0
      for x in list:
          mapi_list += [f(x, i)]
          i += 1
      return mapi_list

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mapi (+) [0; 0; 0; 2; 2; 2];;
 - : int list = [0; 1; 2; 5; 6; 7]
[*----------------------------------------------------------------------------*)
 (*repno rekurzivna*)
let rec mapi f list  (*i*)=  (*i = acc =0 lahko šele kasneje damo notr, f se ne spremeni zato ga kasneje ne rabimo*)
  let rec mapi_aux (*f*)list acc =  (*delamo s match with*)
    match list with 
    | []->[]
    | x::xs -> (f x acc) :: (mapi_aux xs (acc+1)) 
  in 
  mapi_aux list 0

(*----------------------------------------------------------------------------*]
 Funkcija [zip] sprejme dva seznama in vrne seznam parov istoležnih
 elementov podanih seznamov. Če seznama nista enake dolžine vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # zip [1; 1; 1; 1] [0; 1; 2; 3];;
 - : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)]
 # zip [1; 1; 1; 1] [1; 2; 3; 4; 5];;
 Exception: Failure "Different lengths of input lists.".
[*----------------------------------------------------------------------------*)
(*ni repnorekurz*)
let rec zip list1 list2 = 
  match list1, list2 with 
  | [],[] ->[]
 (* | _,[]| [],_ -> failwith "seznama nista iste dolzine"*)
  | x :: xs, y :: ys -> (x,y)::(zip xs ys)
  |_-> failwith " seznama nista istih dolžin "
  (*če upoštevaš vrstni red izvajanja lahko daš na koncu spološen failwith*)

let rec zip list1 list2 = 
  let rec zip_tlrec acc list1 list2 = 
    match list1, list2 with 
    | x::xs, y::ys -> zip_tlrec ((x,y)::acc) xs ys
    |[],[]-> reverse acc
    |_-> failwith "seznama nista iste dolžine"
  in zip_tlrec [] list1 list2
(*----------------------------------------------------------------------------*]
 Funkcija [unzip] je inverz funkcije [zip], torej sprejme seznam parov
 [(x0, y0); (x1, y1); ...] in vrne par seznamov ([x0; x1; ...], [y0; y1; ...]).
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let rec unzip = function 
  |[] ->([],[])
  |(x,y)::rest -> let (list1, list2) = unzip rest in (x::list1,y::list2)

let rec unzip' = function 
    |[]-> ([], [])
    |(x,y)::xys -> let (xs,ys) = unzip xys in x::xs,y::ys

(*----------------------------------------------------------------------------*]
 Funkcija [unzip_tlrec] je repno rekurzivna različica funkcije [unzip].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip_tlrec [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let rec unzip_tlrec list = 
  let rec unzip_aux list acc1 acc2=  (*dva prazna seznama za accumulatorja *)
    match list with (*lahko bi dali function na list*)
    |[]->(reverse acc1,reverse acc2) (*spet moraš obrnit zaradi vrstnega reda. vidiš,ko nardiš test*)
    |(x,y)::rest -> unzip_aux rest (x::acc1)(y::acc2)
    in unzip_aux list [] []

(*----------------------------------------------------------------------------*]
 Funkcija [loop condition f x] naj se izvede kot python koda:

  def loop(condition, f, x):
      while condition(x):
          x = f(x)
      return x

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # loop (fun x -> x < 10) ((+) 4) 4;;
 - : int = 12
[*----------------------------------------------------------------------------*)
(*while zanko smo pretvorli v if zanko*)
let rec loop condition f x =
  if condition x then 
  loop condition f (f x)
  else x 

(*rekurzija je močnejša od zank*)
(*repna rekurzija je tu nesmiselna ker je prekomplicirana*)

(*----------------------------------------------------------------------------*]
 Funkcija [fold_left_no_acc f list] sprejme seznam [x0; x1; ...; xn] in
 funkcijo dveh argumentov [f] in vrne vrednost izračuna
 f(... (f (f x0 x1) x2) ... xn).
 V primeru seznama z manj kot dvema elementoma vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # fold_left_no_acc (^) ["F"; "I"; "C"; "U"; "S"];;
 - : string = "FICUS"
[*----------------------------------------------------------------------------*)


(*javi OPOZORILO, kako popravit ? *)
(*let rec fold_left_no_acc f list = 
  match list with 
  |[]-> failwith "prazen"  
  |x::xs ->
    match xs with 
    |y::[]-> f x y
    |y::ys when ys!=[]-> let lst = (f x y)::ys in fold_left_no_acc f lst*)
  

 

(*----------------------------------------------------------------------------*]
 Funkcija [apply_sequence f x n] vrne seznam zaporednih uporab funkcije [f] na
 vrednosti [x] do vključno [n]-te uporabe, torej
 [x; f x; f (f x); ...; (f uporabljena n-krat na x)].
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # apply_sequence (fun x -> x * x) 2 5;;
 - : int list = [2; 4; 16; 256; 65536; 4294967296]
 # apply_sequence (fun x -> x * x) 2 (-5);;
 - : int list = []
[*----------------------------------------------------------------------------*)

let rec apply_sequence f x n = 
  let rec apply_seq_aux x n acc =
    if n<0 then []   (* najprej vrni za neustrezne n*)
    else if n>0 then apply_seq_aux (f x) (n-1) (acc@[f x])  (*indukcijski korak*)
    else acc (* vrni kar si dobil na koncu,, ko je n===0*)
  in apply_seq_aux x n [x] (*če začneš z [] se zakomplicira pri n===0*)


(*----------------------------------------------------------------------------*]
 Funkcija [filter f list] vrne seznam elementov [list], pri katerih funkcija [f]
 vrne vrednost [true].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # filter ((<)3) [0; 1; 2; 3; 4; 5];;
 - : int list = [4; 5]
[*----------------------------------------------------------------------------*)

let rec filter f list =
  let rec aux list acc= 
  match list with 
  |x::xs -> 
    if f x != true then aux xs acc
    else aux xs (acc@[x])   (* VPR. ali je boljš tako ali pol klicat reverse funkcijo ?*)
  |[] -> acc
  in aux list [] 

(*----------------------------------------------------------------------------*]
 Funkcija [exists] sprejme seznam in funkcijo, ter vrne vrednost [true] čim
 obstaja element seznama, za katerega funkcija vrne [true] in [false] sicer.
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # exists ((<) 3) [0; 1; 2; 3; 4; 5];;
 - : bool = true
 # exists ((<) 8) [0; 1; 2; 3; 4; 5];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec exists f list=
  let rec aux list acc= 
    match list with 
    |[] -> acc
    |x::xs -> if (f x == acc) then aux xs acc
    else f x
  in aux list false
(*----------------------------------------------------------------------------*]
 Funkcija [first f default list] vrne prvi element seznama, za katerega
 funkcija [f] vrne [true]. Če takšnega elementa ni, vrne [default].
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # first ((<) 3) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 5
 # first ((<) 8) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 0
[*----------------------------------------------------------------------------*)

let rec first f default list =
  let rec aux list acc = 
    match list with
    |[]-> default
    |x::xs -> 
      if f x == acc then aux xs acc
      else x
    in aux list false