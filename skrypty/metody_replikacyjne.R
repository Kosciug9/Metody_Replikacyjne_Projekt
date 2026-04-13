#Przygotowanie
#Funkcja do wczytania pliku, usuniecia niepotrzebnej kolumny i zmianie nazwy jednej kolumny
przygotowanie <- function(plik, nazwa){
  ramka <- read.csv2(plik, sep=";")
  ramka <- ramka[,-4]
  names(ramka)[3] <- nazwa
  return(ramka)
}

slaskie_gest <- przygotowanie("C:\\Users\\emilk\\Desktop\\MR-projekt\\Metody_Replikacyjne_Projekt\\data\\slaskieGestosc.csv","Gestosc")
warmmaz_gest <- przygotowanie("C:\\Users\\emilk\\Desktop\\MR-projekt\\Metody_Replikacyjne_Projekt\\data\\warmmazGestosc.csv","Gestosc")

slaskie_ludnosc <- przygotowanie("C:\\Users\\emilk\\Desktop\\MR-projekt\\Metody_Replikacyjne_Projekt\\data\\slaskieLudnosc.csv","Liczba_Ludnosci")
warmmaz_ludnosc <- przygotowanie("C:\\Users\\emilk\\Desktop\\MR-projekt\\Metody_Replikacyjne_Projekt\\data\\warmmazLudnosc.csv","Liczba_Ludnosci")

slaskie_Mieszkania <- przygotowanie("C:\\Users\\emilk\\Desktop\\MR-projekt\\Metody_Replikacyjne_Projekt\\data\\slaskieMieszkania.csv","Liczba_Mieszkan")
warmmaz_Mieszkania <- przygotowanie("C:\\Users\\emilk\\Desktop\\MR-projekt\\Metody_Replikacyjne_Projekt\\data\\warmmazMieszkania.csv","Liczba_Mieszkan")

slaskie_Sport <- przygotowanie("C:\\Users\\emilk\\Desktop\\MR-projekt\\Metody_Replikacyjne_Projekt\\data\\slaskieSport.csv","Ilosc_Obiektow")
warmmaz_Sport <- przygotowanie("C:\\Users\\emilk\\Desktop\\MR-projekt\\Metody_Replikacyjne_Projekt\\data\\warmmazSport.csv","Ilosc_Obiektow")

slaskie_Wynagrodzenia <- przygotowanie("C:\\Users\\emilk\\Desktop\\MR-projekt\\Metody_Replikacyjne_Projekt\\data\\slaskieWynagrodzenia.csv","Wynagrodzenia")
warmmaz_Wynagrodzenia <- przygotowanie("C:\\Users\\emilk\\Desktop\\MR-projekt\\Metody_Replikacyjne_Projekt\\data\\warmmazWynagrodzenia.csv","Wynagrodzenia")



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
  Średnia = c(
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


# Testy permutacjune a testy klasyczne
# Porównuję wartość wartości oczekiwanej (w tym przypadku reprezentować ją bedzie średnia) i wariancji.

wyniki_klasyczne <- data.frame(
  Zmienna = c("Gęstość zaludnienia","Mieszkania","Obiekty sportowe","Wynagrodzenia"),
  Średnia_perm = wyniki_perm$Średnia,
  Średnia_klas = c(
    t.test(slaskie_gest$Gestosc,warmmaz_gest$Gestosc)$p.value,
    t.test(slaskie_Mieszkania$Liczba_Mieszkan,warmmaz_Mieszkania$Liczba_Mieszkan)$p.value,
    t.test(slaskie_Sport$Ilosc_Obiektow,warmmaz_Sport$Ilosc_Obiektow)$p.value,
    t.test(slaskie_Wynagrodzenia$Wynagrodzenia,warmmaz_Wynagrodzenia$Wynagrodzenia)$p.value
  ),
  Wariancja_perm = wyniki_perm$Wariancja,
  Wariancja_klas = c(
    var.test(slaskie_gest$Gestosc,warmmaz_gest$Gestosc)$p.value,
    var.test(slaskie_Mieszkania$Liczba_Mieszkan,warmmaz_Mieszkania$Liczba_Mieszkan)$p.value,
    var.test(slaskie_Sport$Ilosc_Obiektow,warmmaz_Sport$Ilosc_Obiektow)$p.value,
    var.test(slaskie_Wynagrodzenia$Wynagrodzenia,warmmaz_Wynagrodzenia$Wynagrodzenia)$p.value
  )
)

wyniki_klasyczne

# Porównanie wartości średnich wykazuje niezgodność w wynikach statystyk, jednak wnioski pozostają takie same, jeśli chodzi o różnice między województwami.
# Już tu możemy zobaczyć, że rozkład naszych danych nie jest idealnie normalny a 21 i 36 to zbyt mała liczba obserwacji, by testy były odporne na brak normalności.
# Znaczącą różnicę możemy zauważyć przy badaniu wariancji. Z klasycznego testu F wnioskujemy, że wariancje między województwami, niezaleznie od zmiennej, są istotnie różne.
# Dowodzi to, że testy permutacujne są tu lepszym wyborem.

# Wybieramy zmienne objaśniające i zmienną objaśnianą

# Tworzę ramkę danych, potrzebną mi do macierzy korelacji. Robie to osobno dla obu województw. Wracam również do "surowej" liczby mieszkań, bez przeliczania na 10 000 mieszkańców.

slaskie <- data.frame(
  Gęstość = slaskie_gest$Gestosc,
  Ludność = slaskie_ludnosc$Liczba_Ludnosci,
  Mieszkania = ((slaskie_Mieszkania$Liczba_Mieszkan/10000)*slaskie_ludnosc$Liczba_Ludnosci),
  Sport = slaskie_Sport$Ilosc_Obiektow,
  Wynagrodzenia = slaskie_Wynagrodzenia$Wynagrodzenia
)

warmmaz <- data.frame(
  Gęstość = warmmaz_gest$Gestosc,
  Ludność = warmmaz_ludnosc$Liczba_Ludnosci,
  Mieszkania = ((warmmaz_Mieszkania$Liczba_Mieszkan/10000)*warmmaz_ludnosc$Liczba_Ludnosci),
  Sport = warmmaz_Sport$Ilosc_Obiektow,
  Wynagrodzenia = warmmaz_Wynagrodzenia$Wynagrodzenia
)

# Macierze korelacji 

library(corrplot)
corrplot(cor(slaskie))
corrplot(cor(warmmaz))

# Dla województwa Śląskiego:
# - zmienne objaśniające: Gęstość, Ludność, Mieszkania
# - zmianna objaśniana: Sport

# k-fold dla województwa śląskiego
# k=9 (36 powiatów, więc po 4 w jednej grupie)

library(dplyr)

errors_slaskie <- data.frame(MAE=NULL, RMSE=NULL, MAPE=NULL)
e <- vector()
n <- nrow(slaskie)
slaskie$k = rep(1:9, each= 4)[1:n]

for(i in 1:9){
  test  <- slaskie[slaskie$k == i,]
  train <- slaskie[slaskie$k != i,]
  
  model <- lm(Sport ~ Gęstość + Ludność + Mieszkania, train)
  p <- predict(model,test)
  
  e <- test$Sport - p
  
  df <- data.frame(
    MAE  = mean(abs(e)),
    RMSE = sqrt(mean(e^2)),
    MAPE = mean(abs(e/test$Sport)) * 100
  )
  
  errors_slaskie <- rbind(errors_slaskie, df)
  
}

errors_slaskie

# Jak widzimy model dla województwa Śląskiego najlepiej prognozuję dla foldów od 2 do 4. Dla tak wyznaczonych zbiorów testowych i treningowych myli się o około 20 obiektów sportowych, co patrząc na MAPE nie jest złym wynikiem.
# Dobra jakość prognozy nie występuje, jednak dla każdego foldu. Fold 9 np. ma błąd MAPE sięgający prawie 200%. Biorąc pod uwagę rozpiętość naszych danych (od 12 do 163) model radzi sobie dobrze przy większych wartościach, czego nie można powiedzieć, jeśli skupiamy się na tych mniejszych.

wyniki_error_slaskie <- data.frame(
  Zmienna = c("MAE","RMSE","MAPE"),
  Średnia = c(
    mean(errors_slaskie$MAE),
    mean(errors_slaskie$RMSE),
    mean(errors_slaskie$MAPE)),
  Odchylenie_standardowe = c(
    sd(errors_slaskie$MAE),
    sd(errors_slaskie$RMSE),
    sd(errors_slaskie$MAPE)
  ),
  Min = c(
    min(errors_slaskie$MAE),
    min(errors_slaskie$RMSE),
    min(errors_slaskie$MAPE)
  ),
  Max = c(
    max(errors_slaskie$MAE),
    max(errors_slaskie$RMSE),
    max(errors_slaskie$MAPE)
  )
)

wyniki_error_slaskie


# Z powyższych statystyk wynika, że błędy są rozłożone dość równomiernie, brak outlinerów (wnioskujemy z tego, że średnie MAE i RMSE nie odbiegają znacznie od siebie).
# Największą wartość ma współczynnik MAPE (przeszło 50%) zarówno, jeśli chodzi o jego średnią jak i odchylenie standardowe. Potwierdza to stwierdzenie, że model prognozuje lepiej dla wyzszych wartość.   


# Dla województwa Warmińsko - Mazurskiego:
# - zmienne objaśniające: Sport, Wynagrodzenia
# - zmienna objaśniana: Ludność

# k-fold dla województwa warmińsko-mazurskiego
# k=7 (21 powiatów, więc po 3 w jednej grupie)

errors_warmmaz <- data.frame(MAE=NULL, RMSE=NULL, MAPE=NULL)
e1 <- vector()
n1 <- nrow(warmmaz)
warmmaz$k = rep(1:7, each= 3)[1:n1]

for(i in 1:7){
  test  <- warmmaz[warmmaz$k == i,]
  train <- warmmaz[warmmaz$k != i,]
  
  model <- lm( Ludność ~ Wynagrodzenia + Sport , train)
  p <- predict(model,test)
  
  e1 <- test$Ludność - p
  
  df <- data.frame(
    MAE  = mean(abs(e1)),
    RMSE = sqrt(mean(e1^2)),
    MAPE = mean(abs(e1/test$Ludność)) * 100
  )
  
  errors_warmmaz <- rbind(errors_warmmaz, df)
  
}

errors_warmmaz

# Dla województwa Warmińsko - Mazurskiego najskuteczniejszą prognozę daję nam fold 3 z jedynie 13% błędem. MAPE jest, jednak znów zróżnicowany dla poszczególnych foldów. Waha się od prognozy bardzo dobrej (13%) do niedokładnej (67%). Po raz kolejny może mieć to związek z rozpiętością danych.

wyniki_error_warmmaz <- data.frame(
  Zmienna = c("MAE","RMSE","MAPE"),
  Średnia = c(
    mean(errors_warmmaz$MAE),
    mean(errors_warmmaz$RMSE),
    mean(errors_warmmaz$MAPE)),
  Odchylenie_standardowe = c(
    sd(errors_warmmaz$MAE),
    sd(errors_warmmaz$RMSE),
    sd(errors_warmmaz$MAPE)
  ),
  Min = c(
    min(errors_warmmaz$MAE),
    min(errors_warmmaz$RMSE),
    min(errors_warmmaz$MAPE)
  ),
  Max = c(
    max(errors_warmmaz$MAE),
    max(errors_warmmaz$RMSE),
    max(errors_warmmaz$MAPE)
  )
)

wyniki_error_warmmaz

# Tutaj również nie mamy problemu z outlinerami w błędach. Średnie MAPE wynosi 36%, co jest lepszym wynikiem w porówaniu z województwem Śląskim.


