(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

let is_root a b = (a*a=b) && (a>0)
    
let pack3 a b c = (a,b,c)

let sum_if_not ()= failwith "dopolni me 1.c"
       

let apply () = failwith "dopolni me1.d"

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


let predavanje = {predmet="Programiranje 1"; vrsta=Predavanja; trajanje= 2}

let urnik_profesor = {urnik= Urnik(1,srecanje{predmet="Predmet1"; vrsta=Vaje;trajanje= 2}; 3,srecanje{predmet="Predmet3";vrsta= Predavanje; trajanje = 1; 6,srecanje{predmet="Predmet6";vrsta=Vaje;trajanje=1})}

(* let je_preobremenjen urnik = 
    match urnik with 
    |Predavanje * trajanje *)

let bogastvo () = failwith "dopolni me"
