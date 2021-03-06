(* ========== Vaja 3: Definicije Tipov  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri modeliranju denarja ponavadi uporabljamo racionalna števila. Problemi se
 pojavijo, ko uvedemo različne valute.
 Oglejmo si dva pristopa k izboljšavi varnosti pri uporabi valut.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Definirajte tipa [euro] in [dollar], kjer ima vsak od tipov zgolj en
 konstruktor, ki sprejme racionalno število.
 Nato napišite funkciji [euro_to_dollar] in [dollar_to_euro], ki primerno
 pretvarjata valuti (točne vrednosti pridobite na internetu ali pa si jih
 izmislite).

 Namig: Občudujte informativnost tipov funkcij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dollar_to_euro;;
 - : dollar -> euro = <fun>
 # dollar_to_euro (Dollar 0.5);;
 - : euro = Euro 0.4305
[*----------------------------------------------------------------------------*)

type euro = Euro of float
let euro_test = Euro 0.4305



type dollar = Dollar of float
let dollar_test = Dollar 0.5


type kuna = Kuna of float
let kuna_test = Kuna 6.

(*pretvorbe*)
let dollar_to_euro  d = 
  match d with
  |Dollar x -> Euro (0.91*.x)

let dollar_to_euro (Dollar x) = Euro (0.91*.x)

let euro_to_dollar ( Euro x)= Dollar ( 1.18*.x)

let euro_to_kuna d = 
  match d with 
  |Euro x -> Kuna(7.53*.x)
  
  (*krajše*)
let euro_to_kuna' (Euro x)= Kuna (7.53*.x)

let kuna_to_euro' (Kuna x)= Euro ( 0.13*.x)


(*----------------------------------------------------------------------------*]
 Definirajte tip [currency] kot en vsotni tip z konstruktorji za jen, funt
 in švedsko krono. Nato napišite funkcijo [to_pound], ki primerno pretvori
 valuto tipa [currency] v funte.

 Namig: V tip dodajte še švicarske franke in se navdušite nad dejstvom, da vas
        Ocaml sam opozori, da je potrebno popraviti funkcijo [to_pound].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # to_pound (Yen 100.);;
 - : currency = Pound 0.007
[*----------------------------------------------------------------------------*)

type currency = 
  |Yen of float|Pound of float|Krona of float|Tolar of float 

(* delaš s floati, nepozabi na pike*)
let to_yen = function
  |Pound x -> Yen (x *. 0.2)
  |Yen x-> Yen x (*moraš dodat še tega, sicer da pattern mach not exhaustive*)
  |Krona x-> Yen(x*. 2.3)
  |Tolar x-> Yen(x*. 3.4)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Želimo uporabljati sezname, ki hranijo tako cela števila kot tudi logične
 vrednosti (bool). To bi lahko rešili tako da uvedemo nov tip, ki predstavlja celo
 število ali logično vrednost, v nadaljevanju pa bomo raje konstruirali nov tip
 seznamov.

 Spomnimo se, da lahko tip [list] predstavimo s konstruktorjem za prazen seznam
 [Nil] (oz. [] v Ocamlu) in pa konstruktorjem za člen [Cons(x, xs)] (oz.
 x :: xs v Ocamlu).
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)
(*----------------------------------------------------------------------------*]
 Definirajte tip [intbool_list] z konstruktorji za:
  1.) prazen seznam,
  2.) člen z celoštevilsko vrednostjo,
  3.) člen z logično vrednostjo.

 Nato napišite testni primer, ki bi predstavljal "[5; true; false; 7]".
[*----------------------------------------------------------------------------*)
(*
type int_or_bool = Int of int | Bool of bool
type ib_list = int_or_bool list*)

type intbool_list=
|Nil
|Int of int * intbool_list
|Bool of bool * intbool_list 

(*sezname gnezdiš. določis vsakemu mestu tip in vrednost.*)
let test= Int(5,Bool(true,Bool(false,Int(7,Nil))))

(* test2 = [true;3;true;false;4;false]*)
let test2 = Bool(true,Int(3,Bool(true,Bool(false,Int(4,Bool(false,Nil))))))

(*--------------------------------------------------------------------------*]
 Funkcija [intbool_map f_int f_bool ib_list] preslika vrednosti [ib_list] v nov
 [intbool_list] seznam, kjer na elementih uporabi primerno od funkcij [f_int]
 oz. [f_bool].
[*----------------------------------------------------------------------------*)

 (* vsakemu elementu v seznamu pripredi ustrezno funkcijsko vrednosti in novo mesto v novem seznamu*)
let rec intbool_map f_int f_bool ib_list=
  match ib_list with
  |Nil -> Nil (*prazen sesnam*)
  |Int(x,ib_list) -> Int(f_int x, intbool_map f_int f_bool ib_list)
  |Bool(x,ib_list) -> Bool(f_bool x, intbool_map f_int f_bool ib_list)
(*preslika vrednost s funkcijo f_  v f_ x in nato preslika še  preostanek seznama 
seznam je gnezden, tako Int(7, ib_list) določa int 7 na prvem mestu in nato preostanek seznama. *)


(*repno rekurzivna*)
let rec intbool_list f_int f_bool = 
   let rec mapper = function (* pomožna funkcija*)
   |Nil -> Nil
   |Int(x,ib_list)-> Int(f_int x, mapper ib_list)
   |Bool(x,ib_list)-> Bool(f_bool x, mapper ib_list)
   in mapper
(*ali morajo biti tukej različne oznake, i, b, ali je lahko samo x ? *)


(*----------------------------------------------------------------------------*]
 Funkcija [intbool_reverse] obrne vrstni red elementov [intbool_list] seznama.
 Funkcija je repno rekurzivna.
[*----------------------------------------------------------------------------*)

let rec intbool_reverse ib_list = 
  let rec aux_reverse acc (*Nil*)= function (*lahko das match ib_list with*) 
    |Nil -> acc
    |Int(x,ib_list) -> aux_reverse  (Int(x,acc)) ib_list
    |Bool(x, ib_list)-> aux_reverse  (Bool(x,acc)) ib_list
  in aux_reverse Nil ib_list

  (*----------------------------------------------------------------------------*]
 Funkcija [intbool_separate ib_list] loči vrednosti [ib_list] v par [list]
 seznamov, kjer prvi vsebuje vse celoštevilske vrednosti, drugi pa vse logične
 vrednosti. Funkcija je repno rekurzivna in ohranja vrstni red elementov.
[*----------------------------------------------------------------------------*)
(*5 ::true:: false :: 2:: 1:: true --> (5::2::1),(true::false::true)*)
(*repno rekurzivno*)
(* en akumulator, patternmaching*)
let rec intbool_separate ib_list =
  let rec ib_sep  ib_list acc = 
    match ib_list with 
    |Nil ->[], []
    |Int(i,ibl)-> ib_sep ibl (Int(i,acc))
    |Bool(b,ibl)-> ib_sep ibl (Bool(b, acc))
  in ib_sep ib_list Nil

(*z dvemi akumulatorji*)
let rec intbool_separate ib_list= 
  let rec sep iacc bacc = function
  |Nil -> (iacc,bacc) (* vrni tuple *)
  |Int(i,ibl) -> sep (i::iacc) bacc ibl (* int i dodal int seznamu, nadaljuj.*)
  |Bool(b,ibl) -> sep iacc (b::bacc) ibl 
  in  sep [][] ( intbool_reverse ib_list)  
  (* ker vedno odvzame in doda na prvo mesto moramo obrnit*)


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Določeni ste bili za vzdrževalca baze podatkov za svetovno priznano čarodejsko
 akademijo "Effemef". Vaša naloga je konstruirati sistem, ki bo omogočil
 pregledno hranjenje podatkov.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Čarodeje razvrščamo glede na vrsto magije, ki se ji posvečajo. Definirajte tip
 [magic], ki loči med magijo ognja, magijo ledu in magijo arkane oz. fire,
 frost in arcane.

 Ko se čarodej zaposli na akademiji, se usmeri v zgodovino, poučevanje ali
 raziskovanje oz. historian, teacher in researcher. Definirajte tip
 [specialisation], ki loči med temi zaposlitvami.
[*----------------------------------------------------------------------------*)

type magic = |Fire |Frost |Arcane 

type specialisation = |Historian |Teacher| Researcher

(*----------------------------------------------------------------------------*]
 Vsak od čarodejev začne kot začetnik, nato na neki točki postane študent,
 na koncu pa SE lahko tudi zaposli.
 Definirajte tip [status], ki določa ali je čarodej:
  a.) začetnik [Newbie],
  b.) študent [Student] (in kateri vrsti magije pripada in koliko časa študira),
  c.) zaposlen [Employed] (in vrsto magije in specializacijo).

 Nato definirajte zapisni tip [wizard] z poljem za ime in poljem za trenuten
 status.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # professor;;
 - : wizard = {name = "Matija"; status = Employed (Fire, Teacher)}
[*----------------------------------------------------------------------------*)

type status = 
|Newbie 
|Student  of magic* int(*Fire|Frost|Arcane*) (*število let*)
|Employed of magic* specialisation(* Historian|teacher|researcher*)


(*zapisni tip.definiramo imena polj*)
type wizard ={  
  name: string ;
  status : status }

let professor1 = {name="Ziga"; status = Employed(Frost, Researcher)}

(*----------------------------------------------------------------------------*]
 Želimo prešteti koliko uporabnikov posamezne od vrst magije imamo na akademiji.
 Definirajte zapisni tip [magic_counter], ki v posameznem polju hrani število
 uporabnikov magije.
 Nato definirajte funkcijo [update counter magic], ki vrne nov števec s
 posodobljenim poljem glede na vrednost [magic].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # update {fire = 1; frost = 1; arcane = 1} Arcane;;
 - : magic_counter = {fire = 1; frost = 1; arcane = 2}
[*----------------------------------------------------------------------------*)
(*zapisni tip*)
type magic_counter = {fire:int; frost: int; arcane:int}



let update mc = function 
  (* |Fire -> {fire=mc.fire +1;frost=mc.frost;arcane=mc.arcane} *)
  |Fire -> {mc with fire=mc.fire +1}
  |Frost -> {mc with frost=mc.frost +1}
  |Arcane -> {mc with arcane=mc.arcane +1}


(*----------------------------------------------------------------------------*]
 Funkcija [count_magic] sprejme seznam čarodejev in vrne števec uporabnikov
 različnih vrst magij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count_magic [professor; professor; professor];;
 - : magic_counter = {fire = 3; frost = 0; arcane = 0}
[*----------------------------------------------------------------------------*)

let rec count_magic wizzs =
  match wizzs with
  |[]-> {arcane=0;fire=0;frost=0}
  |w::ws -> 
    match w.status with
    |Newbie -> (count_magic ws )
    |Student (magic ,_ )-> update (count_magic ws) magic
    |Employed(magic,_ )-> update (count_magic ws) magic

let rec count_magic wizzs= 
  let rec magic_counter counter = function 
  |[] -> counter
  |Newbie::wizz -> magic_counter counter wizz
  |Student(magic,_)::wizz
  |Employed(magic,_)::wizz ->
    let counter' = update counter magic 
    in magic_counter counter' wizz 
  in magic_counter{fire=0;frost=0;arcane=0}

let rec count_magic wizz = 
  let rec magic_counter counter = function 
    |[]->counter
    |w::wizz ->
    (
      match w.status with 
    |Newbie -> magic_counter counter wizz
    |Student(magic,_) 
    |Employed(magic,_) -> 
      let counter' = update counter magic in magic_counter counter' wizz
          )
  in magic_counter {fire=0;frost=0;arcane=0}

(*----------------------------------------------------------------------------*]
 Želimo poiskati primernega kandidata za delovni razpis. Študent lahko postane
 zgodovinar po vsaj treh letih študija, raziskovalec po vsaj štirih letih
 študija in učitelj po vsaj petih letih študija.
 Funkcija [find_candidate magic specialisation wizard_list] poišče prvega
 primernega kandidata na seznamu čarodejev in vrne njegovo ime, čim ustreza
 zahtevam za [specialisation] in študira vrsto [magic]. V primeru, da ni
 primernega kandidata, funkcija vrne [None].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let jaina = {name = "Jaina"; status = Student (Frost, 4)};;
 # find_candidate Frost Researcher [professor; jaina];;
 - : string option = Some "Jaina"
[*----------------------------------------------------------------------------*)

let rec find_candidate = ()