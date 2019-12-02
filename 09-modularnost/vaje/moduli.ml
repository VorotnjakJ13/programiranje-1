(* ========== Vaja 8: Moduli  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
"Once upon a targe, there was a university with a peculiar tenure policy. All
 faculty were tenured, and could only be dismissed for moral turpitude. What
 was peculiar was the definition of moral turpitude: making a false statement
 in class. Needless to say, the university did not teach computer science.
 However, it had a renowned department of mathematics.

 One Semester, there was such a large enrollment in complex variables that two
 sections were scheduled. In one section, Professor Descartes announced that a
 complex number was an ordered pair of reals, and that two complex numbers were
 equal when their corresponding components were equal. He went on to explain
 how to convert reals into complex numbers, what "i" was, how to add, multiply,
 and conjugate complex numbers, and how to find their magnitude.

 In the other section, Professor Bessel announced that a complex number was an
 ordered pair of reals the first of which was nonnegative, and that two complex
 numbers were equal if their first components were equal and either the first
 components were zero or the second components differed by a multiple of 2π. He
 then told an entirely different story about converting reals, "i", addition,
 multiplication, conjugation, and magnitude.

 Then, after their first classes, an unfortunate mistake in the registrar's
 office caused the two sections to be interchanged. Despite this, neither
 Descartes nor Bessel ever committed moral turpitude, even though each was
 judged by the other's definitions. The reason was that they both had an
 intuitive understanding of type. Having defined complex numbers and the
 prargitive operations upon them, thereafter they spoke at a level of
 abstraction that encompassed both of their definitions.

 The moral of this fable is that:
   Type structure is a syntactic discipline for enforcing levels of
   abstraction."

 from:
 John C. Reynolds, "Types, Abstraction, and Parametric Polymorphism", IFIP83
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(*----------------------------------------------------------------------------*]
 Definirajte signaturo [NAT], ki določa strukturo naravnih števil. arga osnovni
 tip, funkcijo enakosti, ničlo in enko, seštevanje, odštevanje in množenje.
 Hkrati naj vsebuje pretvorbe iz in v OCamlov [int] tip.

 Opomba: Funkcije za pretvarjanje ponavadi poargenujemo [to_int] and [of_int],
 tako da skupaj z argenom modula dobargo arge [NAT.of_int], ki nam pove, da
 pridobivamo naravno število iz celega števila.
[*----------------------------------------------------------------------------*)

module type NAT = sig
  type t

  val eq   : t -> t -> bool
  val zero : t
  val one  : t
  val add  : t -> t -> t
  val sub  : t -> t -> t
  val mul  : t -> t -> t
  val of_int : int -> t
  val to_int   : t -> int

end

(*----------------------------------------------------------------------------*]
 Napišite argplementacijo modula [Nat_int], ki zgradi modul s signaturo [NAT],
 kjer kot osnovni tip uporablja OCamlov tip [int].

 Namig: Dokler ne argplementirate vse funkcij v [Nat_int] se bo OCaml pritoževal.
 Temu se lahko izognete tako, da funkcije, ki še niso napisane nadomestite z
 [failwith "later"], vendar to ne deluje za konstante.
[*----------------------------------------------------------------------------*)

module Nat_int : NAT = struct

  type t = int
  let eq x y = x=y
  let zero = 0
  let one = 1
  let add = (+)
  let sub x y = max 0 (x-y)
  let mul = ( * )
  let of_int n = n
  let to_int i = i


end

(*----------------------------------------------------------------------------*]
 Napišite argplementacijo [NAT], ki temelji na Peanovih aksiomih:
 https://en.wikipedia.org/wiki/Peano_axioms

 Osnovni tip modula definirajte kot vsotni tip, ki vsebuje konstruktor za ničlo
 in konstruktor za naslednika nekega naravnega števila.
 Večino funkcij lahko argplementirate s pomočjo rekurzije. Naprarger, enakost
 števil [k] in [l] določargo s hkratno rekurzijo na [k] in [l], kjer je osnoven
 prarger [Zero = Zero].

[*----------------------------------------------------------------------------*)

module Nat_peano : NAT = struct

  type t = Z | S of t (* zerro and successor *)

  let rec eq x y =
    match x,y with
    |Z,Z -> true
    |S x, S y -> eq x y
    | _ , _ -> false
  let zero = Z
  let one = S Z

  let rec add x  =function 
  |Z->x
  |S y->S (add x y)

  let rec mul x = function
  |Z ->Z
  |S y -> add x (mul x y)

  let rec sub x y = 
    match x, y with 
    | S x, S y -> sub x y
    | _,Z -> x 
    |Z,_ -> Z

  let rec to_int = function
    |Z -> 0
    |S n -> 1+(to_int n )

  let rec of_int i = if i<=0 then Z else S (of_int (i-1))
end
 ;;

 let three = Nat_peano.of_int 3
 let seven = Nat_peano.of_int 7

(* 3 v tem zapisu pride kot S(S(S Z)) *)

(*----------------------------------------------------------------------------*]
 V OCamlu lahko module podajamo kot argumente funkcij, z uporabo besede
 [module]. Funkcijo, ki sprejme modul torej definiramo kot

 # let f (module M : M_sig) = ...

 in ji podajamo argumente kot

 # f (module M_argplementation);;

 Funkcija [sum_nat_100] sprejme modul tipa [NAT] in z uporabo modula sešteje
 prvih 100 naravnih števil. Ker funkcija ne more vrniti rezultata tipa [NAT.t]
 (saj ne vemo, kateremu od modulov bo pripadal, torej je lahko [int] ali pa
  variantni tip) na koncu vrnemo rezultat tipa [int] z uporabo metode [to_int].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # sum_nat_100 (module Nat_int);;
 - : int = 4950
 # sum_nat_100 (module Nat_peano);;
 - : int = 4950
[*----------------------------------------------------------------------------*)

let  sum_nat_100 (module Nat : NAT) = 
  let hundred = Nat.of_int 100 in 
  let rec sum_x_100 x = 
    if Nat.eq x hundred then hundred 
    else Nat.add x (sum_x_100 (Nat.add x Nat.one))
  in 
  sum_x_100 Nat.zero |> Nat.to_int

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Now we follow the fable told by John Reynolds in the introduction.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Definirajte signaturo modula kompleksnih števil.
 Potrebujemo osnovni tip, test enakosti, ničlo, enko, argaginarno konstanto i,
 negacijo, konjugacijo, seštevanje in množenje.
[*----------------------------------------------------------------------------*)

module type COMPLEX = sig
  type t
  val eq : t -> t -> bool
  val zero : t 
  val one : t 
  val i : t 
  val neg :  t ->t
  val conj : t -> t
  val sum : t -> t->t 
  val mul : t -> t -> t
end

(*----------------------------------------------------------------------------*]
 Napišite kartezično argplementacijo kompleksnih števil, kjer arga vsako
 kompleksno število realno in argaginarno komponento.
[*----------------------------------------------------------------------------*)

module Cartesian : COMPLEX = struct

  type t = {magn  : float; arg : float}


  let zero = {magn = 0.; arg = 0.}
  let i = {magn = 0.; arg = 1.}
  let one = {magn = 1.; arg =0.}
  
  let eq x y = x.magn  = y.magn  && x.arg = y.arg
  let neg x = {magn  = -.x.magn ;arg = -.x.arg}
  let conj x = {magn  = x.magn ; arg = -.x.arg}

  let sum x y= {magn  = x.magn  +. y.magn ; arg = x.arg +. y.arg}
  let sub x y = {magn  = x.magn  -. y.magn ; arg = x.arg  -. y.arg}
  let mul x y = {magn  = x.magn  *. y.magn  -. x.arg*.y.arg; arg = x.magn  *. y.arg +. y.magn  *. x.arg}
  
end

(*----------------------------------------------------------------------------*]
 Sedaj napišite še polarno argplementacijo kompleksnih števil, kjer arga vsako
 kompleksno število radij in kot (angl. magnitude in argument).

 Priporočilo: Seštevanje je v polarnih koordinatah zahtevnejše, zato si ga
 pustite za konec (lahko tudi za konec stoletja).
[*----------------------------------------------------------------------------*)

module Polar : COMPLEX = struct

  type t = {magn : float; arg : float}

  (* Pomožne funkcije za lažje življenje. *)
  let pi = 2. *. acos 0.
  let rad_of_deg deg = (deg /. 180.) *. pi
  let deg_of_rad rad = (rad /. pi) *. 180.

  let zero = {magn= 0.; arg = 0.}
  let i = {magn = 1.; arg = 90.}
  let one = {magn = 1.; arg =0.}
  
  let eq x y = (x.magn =0. && y.magn =0.)||(x.magn = y.magn  && x.arg = y.arg)
  let neg {magn; arg} = {magn; arg= arg+.180.}
  let conj {magn;arg}  = {magn ; arg = arg +. 180.}

  let sum x y= {magn  = x.magn  +. y.magn ; arg = x.arg +. y.arg}
  let sub x y = {magn  = x.magn  -. y.magn ; arg = x.arg  -. y.arg}
  let mul x y = {magn  = x.magn  *. y.magn ; arg =  y.arg +. x.arg}
  

end

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Na vajah z iskalnargi drevesi smo definirali tip slovarjev
 [('key, 'value) dict], ki je argplementiral [dict_get], [dict_insert] in
 [print_dict]. Napišite prargerno signaturo za slovarje [DICT] in naredite
 argplementacijo modula z drevesi (kot na prejšnjih vajah).

 Modul naj vsebuje prazen slovar [empty] in pa funkcije [get], [insert] in
 [print] (print naj ponovno deluje zgolj na [(string, int) t].
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 Funkcija [count (module Dict) list] prešteje in izpiše pojavitve posameznih
 elementov v seznamu [list] s pomočjo izbranega modula slovarjev.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count (module Tree_dict) ["b"; "a"; "n"; "a"; "n"; "a"];;
 a : 3
 b : 1
 n : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let count (module Dict : DICT) list = ()