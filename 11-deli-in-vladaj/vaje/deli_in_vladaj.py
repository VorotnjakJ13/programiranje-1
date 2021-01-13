import random 

###############################################################################
# Želimo definirati pivotiranje na mestu za tabelo [a]. Ker bi želeli
# pivotirati zgolj dele tabele, se omejimo na del tabele, ki se nahaja med
# indeksoma [start] in [end] (vključujoč oba robova).
#
# Primer: za [start = 1] in [end = 7] tabelo
#
#     [10, 4, 5, 15, 11, 2, 17, 0, 18]
#
# preuredimo v
#
#     [10, 0, 2, 4, 11, 5, 17, 15, 18]
#
# (Možnih je več različnih rešitev, pomembno je, da je element 4 pivot.)
#
# Sestavi funkcijo [pivot(a, start, end)], ki preuredi tabelo [a] tako, da bo
# element [ a[start] ] postal pivot za del tabele med indeksoma [start] in
# [end]. Funkcija naj vrne indeks, na katerem je po preurejanju pristal pivot.
# Funkcija naj deluje v času O(n), kjer je n dolžina tabele [a].
#
# Primer:
#
#     >>> a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#     >>> pivot(a, 1, 7)
#     3
#     >>> a
#     [10, 0, 2, 4, 11, 5, 17, 15, 18]
###############################################################################

def pivot1(a, start, fin):
    if start >= end: return start
    
    p = a[start]  #pivot
    prvi_vecji = start + 1
    for i in range(start + 1, fin + 1):
        if a[i] < p:
            a[i], a[prvi_vecji] = a[prvi_vecji], a[i]
            #med [prvi_vecji] in [i] so samo elmnti manjsi od [p].
            prvi_vecji += 1
        p, a[prvi_vecji - 1] = a[prvi_vecji - 1], p
        #vstavimo pivot na pravo mesto.
    return prvi_vecji-1 # indeks pivota


def pivot(a,start, end ) :
    if end <= start :
        return start
   
    first_larger = start + 1
    for i in range(start, end+1):
       if a[i] <a[start]:
            a[first_larger], a[i] = a[i] , a[first_larger]
            first_larger += 1 

    a[start], a[first_larger-1] = a[first_larger-1], a[start]

    return first_larger-1




###############################################################################
# V tabeli želimo poiskati vrednost k-tega elementa po velikosti.
#
# Primer: Če je
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#
# potem je tretji element po velikosti enak 5, ker so od njega manši elementi
#  2, 3 in 4. Pri tem štejemo indekse od 0 naprej, torej je "ničti" element 2.
#
# Sestavite funkcijo [kth_element(a, k)], ki v tabeli [a] poišče [k]-ti element
# po velikosti. Funkcija sme spremeniti tabelo [a]. Cilj naloge je, da jo
# rešite brez da v celoti uredite tabelo [a].
###############################################################################


###############################################################################
# Tabelo a želimo urediti z algoritmom hitrega urejanja (quicksort).
#
# Napišite funkcijo [quicksort(a)], ki uredi tabelo [a] s pomočjo pivotiranja.
# Poskrbi, da algoritem deluje 'na mestu', torej ne uporablja novih tabel.
#
# Namig: Definirajte pomožno funkcijo [quicksort_part(a, start, end)], ki
#        uredi zgolj del tabele [a].
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#     >>> quicksort(a)
#     >>> a
#     [2, 3, 4, 5, 10, 11, 15, 17, 18]
###############################################################################

def quicksort(a):
    def qsort (a,start,end):
        if end<= start:
            return 
        p_i= pivot(a,start, end)
        #sort smaller than pivot
        qsort(a,start, p_i-1)
        #sort bigger than pivot
        qsort(a,p_i+1, end)
    qsort(a, 0 ,len (a)-1)

def test_quicksort():
    for _ in range(1000):
        a=[random.randint(-1000,100000)]
        b1=a[:] # kopija seznama a, neodvisna kasneje od a 
        b2=a[:]
        quicksort(b1)
        b2.sort()
        if b1!=b2 :
            return "Not working, try {}".format(a) 


###############################################################################
# Če imamo dve urejeni tabeli, potem urejeno združeno tabelo dobimo tako, da
# urejeni tabeli zlijemo. Pri zlivanju vsakič vzamemo manjšega od začetnih
# elementov obeh tabel. Zaradi učinkovitosti ne ustvarjamo nove tabele, ampak
# rezultat zapisujemo v že pripravljeno tabelo (ustrezne dolžine).
#
# Funkcija naj deluje v času O(n), kjer je n dolžina tarčne tabele.
#
# Sestavite funkcijo [merge(target, list_1, list_2)], ki v tabelo [target]
# zlije tabeli [list_1] in [list_2]. V primeru, da sta elementa v obeh tabelah
# enaka, naj bo prvi element iz prve tabele.
#
# Primer:
#
#     >>> list_1 = [1, 3, 5, 7, 10]
#     >>> list_2 = [1, 2, 3, 4, 5, 6, 7]
#     >>> target = [-1 for _ in range(len(list_1) + len(list_2))]
#     >>> merge(target, list_1, list_2)
#     >>> target
#     [1, 1, 2, 3, 3, 4, 5, 5, 6, 7, 7, 10]
#
###############################################################################
def zlij(target,begin,end, list1,list2):
    target = [-1 for _ in range(len(list1)+len(list2))]
    i=0
    for target[i] in target:
        if list1[i]<= list2[i]: 
            target[i]= list1[i]
            target[i+1]= list2[i]
        else: 
            target[i]= list2[i]
            target[i+1]= list1[i]
        i+=2



###############################################################################
# Tabelo želimo urediti z zlivanjem (merge sort). Tabelo razdelimo na polovici,
# ju rekurzivno uredimo in nato zlijemo z uporabo funkcije [zlij].
#
# Namig: prazna tabela in tabela z enim samim elementom sta vedno urejeni.
#
# Napišite funkcijo [mergesort(a)], ki uredi tabelo [a] s pomočjo zlivanja. Za
# razliko od hitrega urejanja tu tabele lahko kopirate, zlivanje pa je potrebno
# narediti na mestu.
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#     >>> mergesort(a)
#     >>> a
#     [2, 3, 4, 5, 10, 11, 15, 17, 18]
###############################################################################
