# RE2017_12
Pattern memory


Potrebno je realizovati igru Pattern memory (http://www.primarygames.com/puzzles/memory/patternmemory/). 
Tabla se sastoji iz matrice dimenzija N×N, pri čemu su minimalne dimenzije 3×3. Potrebno je podržati
minimum 5 različitih pattern-a (5 nivoa). Igrica se sastoji u tome da se najpre u određenom
vremenskom intervalu (tshow) na ekranu prikazuje zadati pattern, koji se nakon isteka tog
vremena uklanja nakon čega korisnik pogađa zadati pattern. Pogađanje pattern-a korisnik
obavlja tako što se kroz matricu kreće pomoću 4 tastera na tastaturi, i koristi dodatni taster za
potvrdu polja za koje misli da pripada pattern-u. Polje koje bi se trenutno selektovalo pritiskom
tastera za potvrdu, treba da bude obojeno nekom bojom koja se razlikuje od boje kojom se
prikazuje pattern, a koje se nakon pritiska tastera za potrvdu boji u boju koja se koristi za
prikazivanje pattern-a. Nakon što je koristik K puta, pritiskom tastera za potvrdu izabrao željena polja, 
proverava se uneti pattern. Ukoliko je pattern koji je korisnik uneo, jednak onom
koji je inicijalno prikazan, korisnik dobija poene, i prelazi se na sledeći nivo, dok se u suprotnom
završava igra. Broj K opciono moºe da varira u zavisnosti od nivoa i predstavlja broj elemenata
u prikazanom pattern-u. tshow je proizvoljan razuman vremenski interval koji takođe opciono
može varirati između nivoa. Broj N se definiše unapred i isti je za svaki nivo. Takođe, treba
obratiti pažnju da kada je korisnik označio određeno polje, ukoliko pokuša da ga u istom nivou
označi ponovo, ne treba ništa raditi.

Više na: http://tnt.etf.bg.ac.rs/~oe3re/Projekti/Projekti_2017.pdf
