#!/usr/bin/env python
# coding: utf-8

# In[5]:


import json
from datetime import datetime, date

def wind(plik_tekstowy, przeterminowane_json, annual_rate=0.55, report_date=date.today()):
    przeterminowane = []
    numer = []
    termin = []
    kwota = []
    imie_nazwisko = []
    znaki_specjalne = ['!', '%', '$', '#']
    zepsute = 0

    with open(plik_tekstowy, 'r', encoding='utf8') as plik_tekstowy:
        tekst = plik_tekstowy.readlines()

        for linia in tekst:
            indeksy_znalezionych = []

            for i, char in enumerate(linia):
                if char in znaki_specjalne:
                    indeksy_znalezionych.append(i)

            if len(indeksy_znalezionych) != 8 or linia[indeksy_znalezionych[0]] != linia[indeksy_znalezionych[1]] or linia[indeksy_znalezionych[2]] != linia[indeksy_znalezionych[3]] or linia[indeksy_znalezionych[4]] != linia[indeksy_znalezionych[5]] or linia[indeksy_znalezionych[6]] != linia[indeksy_znalezionych[7]] :
                zepsute += 1
                continue  # Przejdź do kolejnej iteracji pętli

            if indeksy_znalezionych:
                ostatni_indeks = 0  # Ustawienie kursora

                for indeks in indeksy_znalezionych:
                    tekst_pomiedzy = linia[ostatni_indeks + 1:indeks].strip()  # Wyodrębnienie tekstu pomiędzy znakami specjalnymi
                    ostatni_indeks = indeks  # Ustawienie kursora

                    if linia[indeks] == '!':
                        numer.append(tekst_pomiedzy)
                    elif linia[indeks] == '%':
                        termin.append(tekst_pomiedzy)
                    elif linia[indeks] == '$':
                        kwota.append(tekst_pomiedzy)
                    elif linia[indeks] == '#':
                        imie_nazwisko.append(tekst_pomiedzy)

        # Filtruj puste wartości i zmień format zmiennych
        numer_clear = [niepusty_numer for niepusty_numer in numer if niepusty_numer]
        termin_clear = [niepusty_termin for niepusty_termin in termin if niepusty_termin]
        terminy_dat = [datetime.strptime(data_str, '%Y-%m-%d').date() for data_str in termin_clear]
        kwota_clear = [niepusta_kwota for niepusta_kwota in kwota if niepusta_kwota]
        kwota_float = [float(wartosc.replace(' zł', '')) for wartosc in kwota_clear]
        imie_nazwisko_clear = [niepuste_imie for niepuste_imie in imie_nazwisko if niepuste_imie]

        przeterminowane_liczba = 0  # Liczba przeterminowanych spraw
        niezakonczone = 0  # Liczba niewykonanych spraw

        for i, data_terminu in enumerate(terminy_dat):
            if data_terminu < report_date:
                dni_opoznienia = (report_date - data_terminu).days
                odsetki = dni_opoznienia / 365 * annual_rate * kwota_float[i]

                # Tworzenie informacji o sprawie przeterminowanej
                informacje_sprawa = {
                    "numer sprawy": numer_clear[i],
                    "imie i nazwisko": imie_nazwisko_clear[i],
                    "termin": str(data_terminu),
                    "kwota zaleglosci": odsetki + kwota_float[i]
                }
                przeterminowane.append(informacje_sprawa)
                przeterminowane_liczba += 1
            else:
                niezakonczone += 1

        # Zapis listy słowników do pliku JSON
        with open(przeterminowane_json, 'w') as plik_json:
            json.dump(przeterminowane, plik_json)

        return przeterminowane_liczba, niezakonczone, zepsute

# przykład Wywołanie funkcji
#wynik = wind("C:\\Users\\karol\OneDrive\\windyk2uszk.txt", "wynikowy_plik.json", annual_rate=0.55,)
#print(wynik)

#with open('wynikowy_plik.json', 'r') as json_file:
#    lista = json.load(json_file)

#print(lista)







