#Przygotowanie
#Funkcja do wczytania pliku, usuniecia niepotrzebnej kolumny i zmianie nazwy jednej kolumny
przygotowanie <- function(plik, nazwa){
  ramka <- read.csv2(plik, sep=";")
  ramka <- ramka[,-4]
  names(ramka)[3] <- nazwa
  return(ramka)
}

slaskie_gest <- przygotowanie("data/slaskieGestosc.csv","Gestosc")
warmmaz_gest <- przygotowanie("data/warmmazGestosc.csv","Gestosc")

slaskie_ludnosc <- przygotowanie("data/slaskieLudnosc.csv","Liczba_Ludnosci")
warmmaz_ludnosc <- przygotowanie("data/warmmazLudnosc.csv","Liczba_Ludnosci")

slaskie_Mieszkania <- przygotowanie("data/slaskieMieszkania.csv","Liczba_Mieszkan")
warmmaz_Mieszkania <- przygotowanie("data/warmmazMieszkania.csv","Liczba_Mieszkan")

slaskie_Sport <- przygotowanie("data/slaskieSport.csv","Ilosc_Obiektow")
warmmaz_Sport <- przygotowanie("data/warmmazSport.csv","Ilosc_Obiektow")

slaskie_Wynagrodzenia <- przygotowanie("data/slaskieWynagrodzenia.csv","Wynagrodzenia")
warmmaz_Wynagrodzenia <- przygotowanie("data/warmmazWynagrodzenia.csv","Wynagrodzenia")

#Dane na rok 2022 dla Powiatów województwa Śląskiego i Warmińsko-mazurskiego
#Dane z Banku Danych Lokalnych GUS
#Zmienne:

#1.Gęstość zaludnienia (ilość ludzi na 1 km^2)
# slaskie_gest, warmmaz_gest

#2.Liczba ludności (pomocnicza zmienna do przeliczania wskaźników)
#slaskie_ludnosc, warmmaz_ludnosc

#3.Liczba lokali mieszkalnych sprzedanych w ramach transakcji rynkowych
#sklaskie_Mieszkania, warmmaz_Mieszkania
#(pozwolę sobie przekształcić zmienną do postaci wskaźnika na mieszkańca)

#4.Liczba obiektów sportowych ogółem
#slaskie_Sport, warmmaz_Sport
#(pozwolę sobie przekształcić zmienną do postaci wskaźnika na mieszkańca)

#5.Przeciętne miesięczne wynagrodzenie brutto
#slaskie_Wynagrodzenia, warmmaz_Wynagrodzenia


#Przeliczenie na wskaźniki na  10 000 mieszkańców
slaskie_Mieszkania$Liczba_Mieszkan <- slaskie_Mieszkania$Liczba_Mieszkan / slaskie_ludnosc$Liczba_Ludnosci * 10000
warmmaz_Mieszkania$Liczba_Mieszkan <- warmmaz_Mieszkania$Liczba_Mieszkan / warmmaz_ludnosc$Liczba_Ludnosci * 10000



#Testy permutacyjne
set.seed(999)

#Dla wariancji
test_perm_var <- function(x,y){
  z <- c(x,y)
  n1 <- length(x)
  n2 <- length(y)
  
  s <- var(x)/var(y)
  if(s<1) s = 1/s
  
  smp <- sapply(1:10000, sample, x=z, size=n1+n2)
  
  stat <- vector()
  for(i in 1:10000){
    a <- smp[1:n1,i]
    b <- smp[(n1+1):(n1+n2),i]
    if(var(a)>var(b)){
      stat[i] <- var(a)/var(b)
    } else{
      stat[i] <- var(b)/var(a)
    }
  }
  return(sum(stat>s)/10000)
}

#Dla wartości oczekiwanych,skośności i kurtozy
library(moments)
test_perm <- function(x,y,s){
  z <- c(x,y)
  n1 <- length(x)
  n2 <- length(y)
  m <- abs((s(x) - s(y)))
  
  smp <- sapply(1:10000, sample, x=z, size = n1+n2)
  
  stat <- vector()
  for(i in 1:10000){
    a <- smp[1:n1,i]
    b <- smp[(n1+1):(n1+n2),i]
    
    stat[i] <- abs(s(a)-s(b))
  }
  
  return(sum(stat > m)/10000)
}

wyniki_perm <- data.frame(
  Zmienna = c("Gęstość zaludnienia","Mieszkania","Obiekty sportowe","Wynagrodzenia"),
  Srednia = c(
    test_perm(slaskie_gest$Gestosc,warmmaz_gest$Gestosc,mean),
    test_perm(slaskie_Mieszkania$Liczba_Mieszkan,warmmaz_Mieszkania$Liczba_Mieszkan,mean),
    test_perm(slaskie_Sport$Ilosc_Obiektow,warmmaz_Sport$Ilosc_Obiektow,mean),
    test_perm(slaskie_Wynagrodzenia$Wynagrodzenia,warmmaz_Wynagrodzenia$Wynagrodzenia,mean)
  ),
  Wariancja = c(
    test_perm_var(slaskie_gest$Gestosc,warmmaz_gest$Gestosc),
    test_perm_var(slaskie_Mieszkania$Liczba_Mieszkan,warmmaz_Mieszkania$Liczba_Mieszkan),
    test_perm_var(slaskie_Sport$Ilosc_Obiektow,warmmaz_Sport$Ilosc_Obiektow),
    test_perm_var(slaskie_Wynagrodzenia$Wynagrodzenia,warmmaz_Wynagrodzenia$Wynagrodzenia)
  ),
  Skośność = c(
    test_perm(slaskie_gest$Gestosc,warmmaz_gest$Gestosc,skewness),
    test_perm(slaskie_Mieszkania$Liczba_Mieszkan,warmmaz_Mieszkania$Liczba_Mieszkan,skewness),
    test_perm(slaskie_Sport$Ilosc_Obiektow,warmmaz_Sport$Ilosc_Obiektow,skewness),
    test_perm(slaskie_Wynagrodzenia$Wynagrodzenia,warmmaz_Wynagrodzenia$Wynagrodzenia,skewness)
  ),
  Kurtoza = c(
    test_perm(slaskie_gest$Gestosc,warmmaz_gest$Gestosc,kurtosis),
    test_perm(slaskie_Mieszkania$Liczba_Mieszkan,warmmaz_Mieszkania$Liczba_Mieszkan,kurtosis),
    test_perm(slaskie_Sport$Ilosc_Obiektow,warmmaz_Sport$Ilosc_Obiektow,kurtosis),
    test_perm(slaskie_Wynagrodzenia$Wynagrodzenia,warmmaz_Wynagrodzenia$Wynagrodzenia,kurtosis)
  )
  
)

wyniki_perm
#Wyniki testów wskazują, że dla gęstości zaludnienia oraz wynagrodzeń występują istotne różnice między województwami
#dla praktycznie każdej cechy rozkładu
#Za to dla zmiennych Mieszkania, Obiekty Sportowe test nie wykazał istotnie statystycznych różnic
#i sugeruje że mamy większe podobieństwo ich rozkładów dla badanych regionów.



