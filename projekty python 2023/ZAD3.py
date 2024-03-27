import datetime as dt

def date_difference(start_date, end_date):
    if start_date < end_date:
        return (end_date - start_date).days
    else:
        print("Data zakończenia jest wcześniejsza niż data rozpoczęcia.")
        return None

class Osoba:
    def __init__(self, imiona, nazwisko):
        self.imiona = imiona
        self.nazwisko = nazwisko

class Ubezpiecz():
    def __init__(self, osoba):
        self.osoba = osoba
        self.imiona = osoba.imiona
        self.nazwisko = osoba.nazwisko

class Klient():
    _nrKlienta = 111111129

    def __init__(self, osoba):
        self.osoba = osoba
        self.imiona = osoba.imiona
        self.nazwisko = osoba.nazwisko
        Klient._nrKlienta += 1
        self.id = Klient._nrKlienta

class PolisaGrupowa:
    _nrPolisy = 123456803

    def __init__(self, klient, cena, okres_ubez, *ubezpieczeni):
        if len(ubezpieczeni) <= 1:
            print("Ubezpieczeniem grupowym muszą być objęte co najmniej dwie osoby")
        elif not isinstance(klient.osoba, Osoba):
            return
        else:
            self.klient = klient
            self.cena = cena
            self.okres_ubez = okres_ubez
            self.ubezpieczeni = list(ubezpieczeni)

            PolisaGrupowa._nrPolisy += 1
            self.id = PolisaGrupowa._nrPolisy

    def ileDni(self):
        return date_difference(self.okres_ubez[0], self.okres_ubez[1])


    def zaSiebie(self):
        return self.klient.osoba in [ubez.osoba for ubez in self.ubezpieczeni]

    def __str__(self):
        data_od = self.okres_ubez[0]
        data_do = self.okres_ubez[1]

        text = f"Polisa:{self.id} ochrona od {data_od} do {data_do} {self.cena} zł\n"
        text += f" Klient {self.klient.id}: {self.klient.nazwisko} {self.klient.imiona}\n"

        ubezpieczeni_text = '\n'.join([f" Ubezpiecz: {ubez.osoba.nazwisko} {ubez.osoba.imiona}" for ubez in self.ubezpieczeni])
        text += ubezpieczeni_text

        return text

class PolisaIndywidualna():
    def __init__(self, klient, cena, okres_ubez, *ubezpieczeni):
        if len(ubezpieczeni) != 1:
            print("Polisa indywidualna obejmuje tylko jedną osobę.")
        elif not isinstance(klient.osoba, Osoba):
            return
        else:
            self.klient = klient
            self.cena = cena
            self.okres_ubez = okres_ubez
            self.ubezpieczeni = list(ubezpieczeni)

            PolisaGrupowa._nrPolisy += 1
            self.id = PolisaGrupowa._nrPolisy

    def ileDni(self):
        return date_difference(self.okres_ubez[0], self.okres_ubez[1])

    def zaSiebie(self):
        return self.klient.osoba == self.ubezpieczeni[0].osoba

    def __str__(self):
        data_od = self.okres_ubez[0]
        data_do = self.okres_ubez[1]

        text = f"Polisa:{self.id} ochrona od {data_od} do {data_do} {self.cena} zł\n"
        text += f" Klient {self.klient.id}: {self.klient.nazwisko} {self.klient.imiona}\n"
        text += f" Ubezpiecz: {self.ubezpieczeni[0].osoba.nazwisko} {self.ubezpieczeni[0].osoba.imiona}"

        return text