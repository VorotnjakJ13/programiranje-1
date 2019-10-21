import csv
import os
import requests
import re
import orodja

###############################################################################
# Najprej definirajmo nekaj pomožnih orodij za pridobivanje podatkov s spleta.
###############################################################################

# definiratje URL glavne strani bolhe za oglase z mačkami
cats_frontpage_url = 'http://www.bolha.com/zivali/male-zivali/macke/'
# mapa, v katero bomo shranili podatke
cat_directory = 'macke'
# ime datoteke v katero bomo shranili glavno stran
frontpage_filename = 'macke_frontpage.html'
# ime CSV datoteke v katero bomo shranili podatke
csv_filename = 'macke.csv'


def download_url_to_string(url):
    '''This function takes a URL as argument and tries to download it
    using requests. Upon success, it returns the page contents as string.'''
    try:
        page_content = requests.get(url).text()
        # del kode, ki morda sproži napako
    
    except requests.exceptions.RequestException as e :
        # koda, ki se izvede pri napaki
        # dovolj je če izpišemo opozorilo in prekinemo izvajanje funkcije
        print(e)
        page_content =""
    # nadaljujemo s kodo če ni prišlo do napake
    return page_content


def save_string_to_file(text, directory, filename):
    """Funkcija zapiše vrednost parametra "text" v novo ustvarjeno datoteko
    locirano v "directory"/"filename", ali povozi obstoječo. V primeru, da je
    niz "directory" prazen datoteko ustvari v trenutni mapi.
    """
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as file_out:
        file_out.write(text)
    return None


# Definirajte funkcijo, ki prenese glavno stran in jo shrani v datoteko.


def save_frontpage(url, directory, filename):
    '''Save "cats_frontpage_url" to the file
    "cat_directory"/"frontpage_filename"'''
    content = download_url_to_string(url)
    directory = cat_directory
    filename = csv_filename
    save_string_to_file(content, directory, filename)
    return None

###############################################################################
# Po pridobitvi podatkov jih želimo obdelati.
###############################################################################


def read_file_to_string(directory, filename):
    '''Return the contents of the file "directory"/"filename" as a string.'''
    with open (csv_filename, 'r') as catsfile:
        data = catsfile.read()
    return data

# Definirajte funkcijo, ki sprejme niz, ki predstavlja vsebino spletne strani,
# in ga razdeli na dele, kjer vsak del predstavlja en oglas. To storite s
# pomočjo regularnih izrazov, ki označujejo začetek in konec posameznega
# oglasa. Funkcija naj vrne seznam nizov.


def page_to_ads(page_content):
    '''Split "page" to a list of advertisement blocks.'''
    #re.compile tvori niz podatkov, definira od kje do kje je en oglas
    #od div clas = ad do div clas = cleas . nato pride naslednji oglas.
    oglas = re.compile(
        r'<dic clas ="ad">(.*?)<div clas = "clear">', flags = re.DOTALL
        )
    oglasi = re.findall(oglas, page_content)
    return oglasi
    #re.findall vrne seznam nizov.


# Definirajte funkcijo, ki sprejme niz, ki predstavlja oglas, in izlušči
# podatke o imenu, ceni in opisu v oglasu.


def get_dict_from_ad_block(oglas):
    '''Build a dictionary containing the name, description and price
    of an ad block.'''
    vzorec_oglasa = re.compile(
        r'<h3><a title = "(?P<ime>.+?)"'
        r'href = "(?P<link>.+?)"'
        r'class = "additionalInfo"><span><b>(?P<rodovnik>.*?)</b></span><div/>'
        r'<div class = "price"><span>(?P<cena>.*?)</span></div>',
        flags = re.DOTALL
    )
    data = re.search(vzorec_oglasa, oglas)
    #re.search(pattern, string,flags=0)
    #pojdi čez niz in poišči vzorec ki se ujema. vrni none če se nič ne ujema
    slovar_oglasov = data.groupdict()
    #.groupdict() ko najdeš vzorec ki se ujema sestavi slovar iz podatkov

    return slovar_oglasov

# Definirajte funkcijo, ki sprejme ime in lokacijo datoteke, ki vsebuje
# besedilo spletne strani, in vrne seznam slovarjev, ki vsebujejo podatke o
# vseh oglasih strani.


def ads_from_file(filename, directory):
    stran_s_podatki = read_file_to_string(filename,directory)
    posamezni_oglasi = page_to_ads (stran_s_podatki)
    for oglas in posamezni_oglasi : 
        seznam_slovarjev= []
        slovar = get_dict_from_ad_block(oglas)
        seznam_slovarjev.append(slovar)
    return seznam_slovarjev


###############################################################################
# Obdelane podatke želimo sedaj shraniti.
###############################################################################


def write_csv(fieldnames, rows, directory, filename):
    """
    Funkcija v csv datoteko podano s parametroma "directory"/"filename" zapiše
    vrednosti v parametru "rows" pripadajoče ključem podanim v "fieldnames"
    """
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)
    return


# Definirajte funkcijo, ki sprejme neprazen seznam slovarjev, ki predstavljajo
# podatke iz oglasa mačke, in zapiše vse podatke v csv datoteko. Imena za
# stolpce [fieldnames] pridobite iz slovarjev.


def write_cat_ads_to_csv(ads, directory, filename):
    """Funkcija vse podatke iz parametra "ads" zapiše v csv datoteko podano s
    parametroma "directory"/"filename". Funkcija predpostavi, da sa ključi vseh
    sloverjev parametra ads enaki in je seznam ads neprazen.

    """
    # Stavek assert preveri da zahteva velja
    # Če drži se program normalno izvaja, drugače pa sproži napako
    # Prednost je v tem, da ga lahko pod določenimi pogoji izklopimo v
    # produkcijskem okolju
    assert ads and (all(j.keys() == ads[0].keys() for j in ads))
    raise NotImplementedError()


# Celoten program poženemo v glavni funkciji

def main(redownload=True, reparse=True):
    """Funkcija izvede celoten del pridobivanja podatkov:
    1. Oglase prenese iz bolhe
    2. Lokalno html datoteko pretvori v lepšo predstavitev podatkov
    3. Podatke shrani v csv datoteko
    """
    # Najprej v lokalno datoteko shranimo glavno stran

    # Iz lokalne (html) datoteke preberemo podatke

    # Podatke prebermo v lepšo obliko (seznam slovarjev)

    # Podatke shranimo v csv datoteko

    # Dodatno: S pomočjo parameteov funkcije main omogoči nadzor, ali se
    # celotna spletna stran ob vsakem zagon prense (četudi že obstaja)
    # in enako za pretvorbo

    raise NotImplementedError()


#if __name__ == '__main__':
#    main()
