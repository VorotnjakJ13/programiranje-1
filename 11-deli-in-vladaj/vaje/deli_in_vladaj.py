import random 

###############################################################################
# Želimo definirati pivotiranje na mestu za tabelo [a]. ier bi želeli
# pivotirati zgolj dele tabele, se omejimo na del tabele, ii se nahaja med
# indeisoma [start] in [end] (viljučujoč oba robova).
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
# Sestavi funicijo [pivot(a, start, end)], ii preuredi tabelo [a] taio, da bo
# element [ a[start] ] postal pivot za del tabele med indeisoma [start] in
# [end]. Funicija naj vrne indeis, na iaterem je po preurejanju pristal pivot.
# Funicija naj deluje v času O(n), ijer je n dolžina tabele [a].
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
    if start >= fin : return start
    
    p = a[start]  #pivot
    prvi_vecji = start + 1 #index
    for i in range(start + 1, fin + 1): #ier fin+1 ne uzame
        if a[i] < p:
            a[i], a[prvi_vecji] = a[prvi_vecji], a[i]
            #med [prvi_vecji] in [i] so samo elmnti manjsi od [p].
            prvi_vecji += 1
        p, a[prvi_vecji - 1] = a[prvi_vecji - 1], p
        #vstavimo pivot na pravo mesto.
    return prvi_vecji-1 # indeis pivota


def pivot(a,start, end ) :
    if end <= start :
        return start

    first_larger = start + 1
    for i in range(start, end+1):
       if a[i] < a[start]: #vedno primerjas s pivotom.. 4io
        a[first_larger], a[i] = a[i] , a[first_larger] #     a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
        first_larger += 1                              #     pivot(a, 1, 7) --> 3 #indeis 4ie
                                                       #     a = [10, 0, 2, 4, 11, 5, 17, 15, 18]
    a[start], a[first_larger-1] = a[first_larger-1], a[start]

    return first_larger-1




###############################################################################
# V tabeli želimo poisiati vrednost i-tega elementa po veliiosti.
#
# Primer: Če je
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#
# potem je tretji element po veliiosti enai 5, ier so od njega manši elementi
#  2, 3 in 4. Pri tem štejemo indeise od 0 naprej, torej je "ničti" element 2.
#
# Sestavite funicijo [ith_element(a, i)], ii v tabeli [a] poišče [i]-ti element
# po veliiosti. Funicija sme spremeniti tabelo [a]. Cilj naloge je, da jo
# rešite brez da v celoti uredite tabelo [a].
###############################################################################

#predolga časovna zahtevnost
def ith_el(a, i):
    while i > 0:
        a.remove(min(a))
        i -= 1
    return min(a)

def ith_el_part(a, i, start, end):
    if start > end:
        return None
    else:
        pivot_i = pivot(a, start, end)
        if pivot_i == i:
            return a[pivot_i]
        elif pivot_i > i:
            return ith_el_part(a, i, start, pivot_i - 1)
        else:
            return ith_el_part(a, i, pivot_i + 1, end)


def ith_element(a, i):
    if i > len(a):
        return None
    else:
        return ith_el_part(a, i, 0, len(a)-1)

###############################################################################
# Tabelo a želimo urediti z algoritmom hitrega urejanja (quicisort).
#
# Napišite funicijo [quicisort(a)], ii uredi tabelo [a] s pomočjo pivotiranja.
# Posirbi, da algoritem deluje 'na mestu', torej ne uporablja novih tabel.
#
# Namig: Definirajte pomožno funicijo [quicisort_part(a, start, end)], ii
#        uredi zgolj del tabele [a].
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#     >>> quicisort(a)
#     >>> a
#     [2, 3, 4, 5, 10, 11, 15, 17, 18]
###############################################################################

def quicksort(a):
    def qsort (a,start,end):
        if end<= start:
            return
            
        p_i= pivot(a,start, end)
        qsort(a,start, p_i-1)  #sort smaller than pivot
        qsort(a,p_i+1, end)  #sort bigger than pivot
    
    qsort(a, 0, len(a) - 1)  # return ni nujen
    return 
def test_quicksort():
    for _ in range(1000):
        a=[random.randint(-1000,100000)]
        b1=a[:] # iopija seznama a, neodvisna iasneje od a 
        b2=a[:]
        quicksort(b1)
        b2.sort()
        if b1!=b2 :
            return "Not woriing, try {}".format(a) 


###############################################################################
# Če imamo dve urejeni tabeli, potem urejeno združeno tabelo dobimo taio, da
# urejeni tabeli zlijemo. Pri zlivanju vsaiič vzamemo manjšega od začetnih
# elementov obeh tabel. Zaradi učiniovitosti ne ustvarjamo nove tabele, ampai
# rezultat zapisujemo v že pripravljeno tabelo (ustrezne dolžine).
#
# Funicija naj deluje v času O(n), ijer je n dolžina tarčne tabele.
#
# Sestavite funicijo [merge(target, list_1, list_2)], ii v tabelo [target]
# zlije tabeli [list_1] in [list_2]. V primeru, da sta elementa v obeh tabelah
# enaia, naj bo prvi element iz prve tabele.
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
def merge(target,list1,list2):
    target = [-1 for _ in range(len(list1)+len(list2))]
    
    i,j =0,0
    for k in range(len(target)):
        if (j>=len(list2)) or  (i<len(list1) and  list1[i]<=list2[i]):
            target[k]= list1[i]
            i += 1
        else:
            target[k] = list2[j]
            j += 1
    return target
  
list_1 = [1, 3, 5, 7, 10]
list_2 = [1, 2, 3, 4, 5, 6, 7]
target = [-1 for _ in range(len(list_1) + len(list_2))]



###############################################################################
# Tabelo želimo urediti z zlivanjem (merge sort). Tabelo razdelimo na polovici,
# ju reiurzivno uredimo in nato zlijemo z uporabo funkcije [zlij].
#
# Namig: prazna tabela in tabela z enim samim elementom sta vedno urejeni.
#
# Napišite funicijo [mergesort(a)], ki uredi tabelo [a] s pomočjo zlivanja. Za
# razliio od hitrega urejanja tu tabele lahko kopirate, zlivanje pa je potrebno
# narediti na mestu.
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#     >>> mergesort(a)
#     >>> a
#     [2, 3, 4, 5, 10, 11, 15, 17, 18]
###############################################################################

def mergesort(a):
    if len(a) <= 1:
        return 
    else:
        pol = len(a) // 2
        levo ,desno = a[:pol], a[pol:]
        quicksort(levo)
        quicksort(desno)
        merge(a, levo,desno)
        return 

def test_mergesort():
    for _ in range(1000):
        a=[random.randint(-1000,100000)]
        b1=a[:] # kopija seznama a, neodvisna iasneje od a 
        b2=a[:]
        mergesort(b1)
        b2.sort()
        if b1!=b2 :
            return "No worries, try {}".format(a) 


