(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

<<<<<<< HEAD
let is_root a b = (a*a=b) && (a>0)
    
let pack3 a b c = (a,b,c)

let sum_if_not ()= failwith "dopolni me 1.c"
       

let apply () = failwith "dopolni me1.d"
=======
let is_root = "dopolni me"

let pack3 = "dopolni me"

let sum_if_not = "dopolni me"

let apply = "dopolni me"
>>>>>>> 681069df792015de7879798df37ca1f93ce57a16

(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

type vrsta_srecanja = Predavanja|Vaje

type srecanje ={
    predmet :string;
    vrsta :vrsta_srecanja;
    trajanje : int;
}
type urnik  = Urnik of int*srecanje

let vaje = {predmet="Analiza 2a";vrsta= Vaje; trajanje = 3}


<<<<<<< HEAD
let predavanje = {predmet="Programiranje 1"; vrsta=Predavanja; trajanje= 2}

let urnik_profesor = {urnik= Urnik(1,srecanje{predmet="Predmet1"; vrsta=Vaje;trajanje= 2}; 3,srecanje{predmet="Predmet3";vrsta= Predavanje; trajanje = 1; 6,srecanje{predmet="Predmet6";vrsta=Vaje;trajanje=1})}

(* let je_preobremenjen urnik = 
    match urnik with 
    |Predavanje * trajanje *)
=======
type urnik = DopolniMe''

let vaje = "dopolni me"
let predavanja = "dopolni me"

let urnik_profesor = "dopolni me"
>>>>>>> 681069df792015de7879798df37ca1f93ce57a16

let je_preobremenjen = "dopolni me"

let bogastvo = "dopolni me"
