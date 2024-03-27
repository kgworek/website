

####################### import danych z pliku CSV ####################### 

library(foreign)

ASC_niesezonowe <- read.csv ("dane/Gworek_ASC_niesezonowe.csv", # nazwa pliku
                             header = TRUE,                      # czy obserwacje w pierwszym wierszu?
                             sep = ";",                          # separator kolumn
                             dec = "," )	                       # separator dziesiatny

is.ts(ASC_niesezonowe)                           


ASC_niesezonowe.ts <- ts(ASC_niesezonowe$rate, start=c(2012,1),  freq = 12 ) 
                     

class(ASC_niesezonowe) 

is.ts(ASC_niesezonowe.ts) 

# próby in-sample i out-of-sample

ASC_niesezonowe.in <- 
  window(ASC_niesezonowe.ts,
         end = c(2022, 09))

ln.ASC_niesezonowe.in  <- log(ASC_niesezonowe.in)

library(xts)
diff.ln.ASC_niesezonowe.in <-diff.xts(ln.ASC_niesezonowe.in )

ASC_niesezonowe.out <- 
  window(ASC_niesezonowe.ts,
         start = c(2022, 10))

ln.ASC_niesezonowe.out <- log(ASC_niesezonowe.out)


                            


#######################  Wizualizacja szeregu ####################### 


plot(ASC_niesezonowe.ts,
     main = "Kurs USD/PLN",
     xlab = "data",
     ylab = "Exchage rate")
    

####################### Model ekstrapolacyjny #########################

# prosty model wygładzania wykładniczego (EWMA) - bez trendu i sezonowości

EWMA <- HoltWinters(ASC_niesezonowe.in,
                       beta  = FALSE, # beta jest czynnikiem trendu
                       gamma = FALSE) # gamma jest czynnikiem sezonowym
EWMA

EWMA.forecast <- predict(EWMA, # obliczenie prognozy na 3 obserwacje do przodu
                            n.ahead = 3,
                            prediction.interval = TRUE)
library(rcompanion)

par(mfrow=c(1,2))

plot(ASC_niesezonowe.ts)
lines(EWMA.forecast[, 1], col = "blue") # prognozy 
lines(EWMA.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności dla prognozy
lines(EWMA.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności dla prognozy
abline(v = 2022+9/12, lty = 2)  # dodajemy pionową linię referencyjną (zeby zobaczyc okres out-of-sample)
title("EWMA")

plot(window(ASC_niesezonowe.ts, start = c(2020, 12)))
lines(EWMA.forecast[, 1], col = "blue") # prognozy 
lines(EWMA.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności dla prognozy
lines(EWMA.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności dla prognozy
abline(v = 2022+9/12, lty = 2)  # dodajemy pionową linię referencyjną 
title("EWMA")



# model Holta

Holt <- HoltWinters(ASC_niesezonowe.in,
                       gamma = FALSE) # gamma jest czynnikiem sezonowym
Holt

Holt.forecast <- predict(Holt, # prognoza na 3 obserwacje do przodu
                            n.ahead = 3,
                            prediction.interval = TRUE)

plot(ASC_niesezonowe.ts)
lines(Holt.forecast[, 1], col = "blue") # prognozy 
lines(Holt.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności dla prognozy
lines(Holt.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności dla prognozy
abline(v = 2022+9/12, lty = 2)  # dodajemy pionową linię referencyjną (zeby zobaczyc okres out-of-sample)
title("Holt")

Holt
plot(window(ASC_niesezonowe.ts, start = c(2020, 12)))
lines(Holt.forecast[, 1], col = "blue") # prognozy 
lines(Holt.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności dla prognozy
lines(Holt.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności dla prognozy
abline(v = 2022+9/12, lty = 2)  # dodajemy pionową linię referencyjną 
title("Holt")




# Porównywanie błędóW prognoz


EWMA$fitted[, 1]
Holt$fitted[, 1]

EWMA.summary <- window(EWMA$fitted[, 1], end =c(2022, 12) , extend = TRUE)
Holt.summary <- window(Holt$fitted[, 1], end =c(2022, 12) , extend = TRUE)

# w miejsce braków danych na końcu wstawiamy prognozy 
window(EWMA.summary, start = c(2022, 10)) <- EWMA.forecast[, 1]
window(Holt.summary, start = c(2022, 10)) <- Holt.forecast[, 1]

EWMA.summary
Holt.summary

# łączymy wszystkie kolumny razem 
niesezonowy.summary <- ts.union(ASC_niesezonowe.ts,EWMA.summary,Holt.summary)

library(xts)
niesezonowy.summary = as.xts(niesezonowy.summary)
ifelse(index(niesezonowy.summary) < "2022-10", 0, 1)

sample_period<-
  ts(ifelse(index(niesezonowy.summary) < "2022-10", 0, 1), 
     start  =c(2012, 1), freq = 12)

names(niesezonowy.summary)


# nazwy trochę uprościmy
names(niesezonowy.summary)<-c("ASC","EWMA","Holt")

# utworzymy też zmienną indeksującą czas (date)
niesezonowy.summary$date <- index(ASC_niesezonowe.ts)

niesezonowy.summary$sample_period <- sample_period

niesezonowy.summary


#porównanie 
sample_period.niesezonowy
niesezonowy.summary$sample_period.niesezonowy <- sample_period.niesezonowy
niesezonowy.summary


niesezonowy.summary$mae_EWMA <- abs(niesezonowy.summary$EWMA-niesezonowy.summary$ASC)
niesezonowy.summary$mse_EWMA   <-(niesezonowy.summary$EWMA-niesezonowy.summary$ASC)^2
niesezonowy.summary$mape_EWMA   <- abs((niesezonowy.summary$EWMA-niesezonowy.summary$ASC)/niesezonowy.summary$ASC)
niesezonowy.summary$amape_EWMA   <- abs((niesezonowy.summary$EWMA-niesezonowy.summary$ASC)/(niesezonowy.summary$EWMA+niesezonowy.summary$ASC))

niesezonowy.summary$mae_Holt   <- abs(niesezonowy.summary$Holt-niesezonowy.summary$ASC)
niesezonowy.summary$mse_Holt      <- (niesezonowy.summary$Holt-niesezonowy.summary$ASC)^2
niesezonowy.summary$mape_Holt     <- abs((niesezonowy.summary$Holt-niesezonowy.summary$ASC)/niesezonowy.summary$ASC)
niesezonowy.summary$amape_Holt    <- abs((niesezonowy.summary$Holt-niesezonowy.summary$ASC)/(niesezonowy.summary$Holt+niesezonowy.summary$ASC))


aggregate(niesezonowy.summary[, 7:14],
          by = list(niesezonowy.summary$sample_period.niesezonowy),
          FUN = function(x) mean(x, na.rm = T))

par(mfrow=c(1,1))






# wczytanie pakietow do pamieci

library(foreign)
library(xts)
library(urca)
library(lmtest)
library(fBasics)
library(forecast)
library(tseries)
library(fUnitRoots)


# wczytanie funkcji do wykorzystania w analizie

library(readxl)
library(zoo)
library(ggplot2)
options(scipen=999)

library(rcompanion)
par(mfrow=c(1,1))







################################################################################
########### DANE NIESEZONOWE - kursu wymiany USA/PLN - MODEL ARIMA  ############
################################################################################


#testy na stacjonarność  


# wykres
plot(ASC_niesezonowe.in,
     type = "l",
     main = "Kursu wymiany USA/PLN")


# pierwsze różnice
library(xts)
diff.ASC_niesezonowe.in<-diff.xts(ASC_niesezonowe.in)

par(mfrow=c(2,1))

plot(diff.ASC_niesezonowe.in,
     type = "l",
     main = "Pierwsze różnice dla kursu wymiany USA/PLN")



#logarytmowanie w celu wyeliminowania zmieniającej się wariancji, ale okazują się niekonieczne 
ln.ASC_niesezonowe.in<-log(ASC_niesezonowe.in)
diff.ln.ASC_niesezonowe.in<-diff.xts(ln.ASC_niesezonowe.in)

plot(diff.ln.ASC_niesezonowe.in,
     type = "l",
     main = "Pierwsze różnice dla logarytmu kursu wymiany USA/PLN")

par(mfrow=c(1,1))


#test na stacjonarność dla ASC_niesezonowego bez różnic 

library(urca)
df.test0 <- ur.df(ASC_niesezonowe.in, type = c("none"), lags = 0)
summary(df.test0)



# sprawdźmy autokorelację reszt
library(lmtest)
resids0 <- df.test0@testreg$residuals
bg1 <- bgtest(resids~ 1, order = 1)
bg1
# p-value = 0.7709 --> brak autokorelacji pierwszych reszt, ale musimy sprawdzić 6 i wtedy wnioski
bg2 <- bgtest(resids0 ~ 1, order = 2)
bg2
bg3 <- bgtest(resids0 ~ 1, order = 3)
bg3
bg4 <- bgtest(resids0 ~ 1, order = 4)
bg4
# p-value=0.014 autokorelacja 
bg5 <- bgtest(resids0 ~ 1, order = 5)
bg5
bg6 <- bgtest(resids0 ~ 1, order = 6)
bg6



# sprawdźmy autokorelację reszt dla rozszerzonego testu trzeba wprowadzić lags=2 bo przy lags= 1 nadal autokorelacja 

df.test01 <- ur.df(ASC_niesezonowe.in, type = c("none"), lags = 2)
summary(df.test01)

library(lmtest)
resids01 <- df.test01@testreg$residuals
bg1 <- bgtest(resids01~ 1, order = 1)
bg1
# p-value = 0.7709 --> brak autokorelacji pierwszych reszt, ale musimy sprawdzić 6 i wtedy wnioski
bg2 <- bgtest(resids01 ~ 1, order = 2)
bg2
bg3 <- bgtest(resids01 ~ 1, order = 3)
bg3
bg4 <- bgtest(resids01 ~ 1, order = 4)
bg4
# p-value=0.036 autokorelacja 
bg5 <- bgtest(resids01 ~ 1, order = 5)
bg5
bg6 <- bgtest(resids01 ~ 1, order = 6)
bg6

# brak autokorelacji można interpretować test DF

summary(df.test01)  
# value of test-statistic is: 1.3503 > -1.95 --> brak podstaw do odrzucenia H0 o niestacjonarności
#potrzebujemy różnic reguralnych



# Test DF przeprowadzimy za pomocą funkcji ur.df() dla pierszych różnic
# c("none", "drift", "trend")
# test DF# uwzględniamy "none" w róWnaniu testującym (wariant 1 testu)
# lags = 0 to liczba rozszerzeń w teście ADF

library(urca)
df.test <- ur.df(diff.ASC_niesezonowe.in[2:129], type = c("none"), lags = 0)
summary(df.test)



# sprawdźmy autokorelację reszt
library(lmtest)
resids <- df.test@testreg$residuals
bg1 <- bgtest(resids~ 1, order = 1)
bg1
# p-value = 0.7709 --> brak autokorelacji pierwszych reszt, ale musimy sprawdzić 6 i wtedy wnioski
bg2 <- bgtest(resids ~ 1, order = 2)
bg2
bg3 <- bgtest(resids ~ 1, order = 3)
bg3
bg4 <- bgtest(resids ~ 1, order = 4)
bg4
# p-value=0.036 autokorelacja 
bg5 <- bgtest(resids ~ 1, order = 5)
bg5
bg6 <- bgtest(resids ~ 1, order = 6)
bg6

# Przechodzimy do testu ADF

# c("none", "drift", "trend")
# test DF# 1 forma funkcujna 
# lags = 1 to liczba rozszerzeń w teście ADF
library(urca)
df.test2 <- ur.df(diff.ASC_niesezonowe.in[2:129], type = c("none"), lags = 1)
summary(df.test2)

# sprawdźmy autokorelację reszt, dane kwartalne sprawdzamy 6 autokorelacje
library(lmtest)
resids2 <- df.test2@testreg$residuals
bg1 <- bgtest(resids2~ 1, order = 1)
bg1
# p-value = 0.9575 --> brak autokorelacji pierwszych reszt, ale musimy sprawdzić 6 i wtedy wnioski
bg2 <- bgtest(resids2 ~ 1, order = 2)
bg2
bg3 <- bgtest(resids2 ~ 1, order = 3)
bg3
bg4 <- bgtest(resids2 ~ 1, order = 4)
bg4
bg5 <- bgtest(resids2 ~ 1, order = 5)
bg5
bg6 <- bgtest(resids2 ~ 1, order = 6)
bg6

# wszytskie p-value>0,05 btak autokorelacji reszt, można interpretować test 
summary(df.test2)
# Value of test-statistic is: -6.9225 < -1.95 --> odrzucamy H0 o niestacjonarności 


# test KPSS
library(urca)

ASC_niesezonowe.in.kpss.test <- ur.kpss(ASC_niesezonowe.in, type = c("mu"))  # stała w równaniu testowym
summary(ASC_niesezonowe.in.kpss.test)

# statystyka testowa (2,1.6934) > 0,463 => odrzucamy H0 o stacjonarnosci ASC_niesezonowe.in
# przeprowadzamy test KPSS dla pierwszych różnic

diff.ASC_niesezonowe.in.kpss.test <- ur.kpss(diff.ASC_niesezonowe.in, type = c("mu"))  # stała w równaniu testowym
summary(diff.ASC_niesezonowe.in.kpss.test)
# statystyka testowa = 0.2004  < 0,463 => brak podstaw do odrzucenia h0 o stacjonarnosci diff.ASC_niesezonowe.in

# stopień integracji zmiennej ASC_niesezonowe.in wynosi 1  (zmienna stacjonarna)
# min I(1) oznacza, że zmienna szeregu czasowego wymaga jednego stopnia różnicowania, aby stać się stacjonarna.




# SPRAWDZAMY CZY ZMIENNA NIE JEST BIAŁYM SZUMEM
# test Ljung-Box
Box.test(diff.ASC_niesezonowe.in, lag=24, type="Ljung-Box") #brak podstaw do odrzucenia H0 o tym, że diff.ASC_niesezonowe.in to bialy szum, poniewaz p-value>0.05 

# test Box-Pierce
Box.test(diff.ASC_niesezonowe.in, lag=24, type="Box-Pierce") #brak podstaw do odrzucenia H0 o tym, że diff.ASC_niesezonowe.in to bialy szum, poniewaz p-value>0.05 

# według testów zmienna jest białym szumem

# sprawdzam korelogramy ACF i PACF
library(forecast)
tsdisplay(diff.ASC_niesezonowe.in, lag.max=24)

par(mfrow = c(1, 2))
Acf(diff.ASC_niesezonowe.in , lag.max = 24,
    lwd = 4, col = "red",
    na.action = na.pass)  
Pacf(diff.ASC_niesezonowe.in , lag.max =24 ,
     lwd = 4, col = "red",
     na.action = na.pass)

# według korelogramów możemy zaproponować model arima (3,1,3) dla zmiennej ASC_niesezonowe.in

# ale dwie pierwsze wypsutki nie są istotne statystycznie, warto zrobic metodę od ogółu do szczegółu

###########################################################################
# III. Estymacja                                                          #
###########################################################################

# Od ogółu do szczegółu 

# 1.
# Zaczynamy od estymacji modelu ARIMA(3,1,3) dla diff.ASC_niesezonowe.in

arima313 <- arima(diff.ASC_niesezonowe.in,  # zmienna zależna
                  order = c(3, 0, 3))  # rzędy (p,d,q)) d=0 bo pierwsze różnice
arima313

coeftest(arima313)

# wartości AIC
AIC(arima313)
# -254.9598

# wartosci BIC
BIC(arima313)
# -232.1436




# estymacji modelu ARIMA(3,1,2) dla diff.ASC_niesezonowe.in
# Testujemy ograniczenie kolejne, użyjemy testu ilorazu wiarogodności
# H0: THETA3=0 ARMA(3,2), statystyka z 1 stopniami swobody, na egzaminie może być do obliczenia 
# statystyka tesotwa 


arima312 <- arima(diff.ASC_niesezonowe.in,  # zmienna zależna
                  order = c(3, 0, 2))  # rzędy (p,d,q)) d=0 bo pierwsze różnice
arima312

coeftest(arima312)

# wartości AIC
AIC(arima312)


# wartosci BIC
BIC(arima312)

# test LR 
# sprawdzamy czy statystyka testowa jest w obszarze ktytycznym tak --> odrzucamy hipotezę 0 - prawastronna hi kwadrat
teststat<- 2*(as.numeric(logLik(arima313))-as.numeric(logLik(arima312)))
teststat

# przyjumuje pozio istotności 1% -->  0.009848097 są podstawy do  orzucenia h0 na poziomie 1%
#0.009848097

# THETA3 nie jest równa 0, zostawiamy q=3, zmniejszamy p 


# H0: alfa3=0 ARMA(2,3), statystyka z 1 stopniami swobody, na egzaminie może być do obliczenia 
# statystyka tesotwa 
# estymacji modelu ARIMA(2,1,3) dla diff.ASC_niesezonowe.in

arima213 <- arima(diff.ASC_niesezonowe.in,  # zmienna zależna
                  order = c(2, 0, 3))  # rzędy (p,d,q)) d=0 bo pierwsze różnice
arima213

coeftest(arima213)

# wartości AIC
AIC(arima213)


# wartosci BIC
BIC(arima213)


teststat<- 2*(as.numeric(logLik(arima313))-as.numeric(logLik(arima213)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )

# przyjumuje pozio istotności 1% -->  0.01415049 brak podstaw do  odrzucenia h0 na poziomie 1%,
# zmniejszamy p=1 



# estymacji modelu ARIMA(1,1,3) dla diff.ASC_niesezonowe.in
# H0: alfa2=alfa3=0 

arima113 <- arima(diff.ASC_niesezonowe.in,  # zmienna zależna
                  order = c(1, 0, 3))  # rzędy (p,d,q)) d=0 bo pierwsze różnice
arima113

coeftest(arima113)

# wartości AIC
AIC(arima113)


# wartosci BIC
BIC(arima113)

# test LR 
# sprawdzamy czy statystyka testowa jest w obszarze ktytycznym tak --> odrzucamy hipotezę 0 - prawastronna hi kwadrat
teststat<- 2*(as.numeric(logLik(arima213))-as.numeric(logLik(arima113)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )

# brak podstaw do odrzucenia h0 --> kończymy procedure na ARIMA (2,1,3)


arima013 <- arima(diff.ASC_niesezonowe.in,  # zmienna zależna
                  order = c(0, 0, 3))  # rzędy (p,d,q)) d=0 bo pierwsze różnice
arima013

coeftest(arima013)

# wartości AIC
AIC(arima013)

# wartosci BIC
BIC(arima013)


# test LR 
# sprawdzamy czy statystyka testowa jest w obszarze ktytycznym tak --> odrzucamy hipotezę 0 - prawastronna hi kwadrat
teststat<- 2*(as.numeric(logLik(arima113))-as.numeric(logLik(arima013)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )

# brak podstaw do odrzucenia h0 


arima212 <- arima(diff.ASC_niesezonowe.in,  # zmienna zależna
                  order = c(2, 0, 2))  # rzędy (p,d,q)) d=0 bo pierwsze różnice
arima212

coeftest(arima212)

# wartości AIC
AIC(arima212)


# wartosci BIC
BIC(arima212)

# test LR 
# sprawdzamy czy statystyka testowa jest w obszarze ktytycznym tak --> odrzucamy hipotezę 0 - prawastronna hi kwadrat
teststat<- 2*(as.numeric(logLik(arima312))-as.numeric(logLik(arima212)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )


# estymacji modelu ARIMA(2,1,1) dla diff.ASC_niesezonowe.in

arima211 <- arima(diff.ASC_niesezonowe.in,  # zmienna zależna
                  order = c(2, 0, 1))  # rzędy (p,d,q)) d=0 bo pierwsze różnice
arima211

coeftest(arima211)

# wartości AIC
AIC(arima211)

# wartosci BIC
BIC(arima211)


# test LR 
# sprawdzamy czy statystyka testowa jest w obszarze ktytycznym tak --> odrzucamy hipotezę 0 - prawastronna hi kwadrat
teststat<- 2*(as.numeric(logLik(arima313))-as.numeric(logLik(arima211)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )

# estymacji modelu ARIMA(1,1,2) dla diff.ASC_niesezonowe.in

arima112 <- arima(diff.ASC_niesezonowe.in,  # zmienna zależna
                  order = c(1, 0, 2))  # rzędy (p,d,q)) d=0 bo pierwsze różnice
arima112

coeftest(arima112)

# wartości AIC
AIC(arima112)
# wartosci BIC
BIC(arima112)


# test LR 
# sprawdzamy czy statystyka testowa jest w obszarze ktytycznym tak --> odrzucamy hipotezę 0 - prawastronna hi kwadrat
teststat<- 2*(as.numeric(logLik(arima312))-as.numeric(logLik(arima112)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )


# estymacji modelu ARIMA(1,1,1) dla diff.ASC_niesezonowe.in

arima111 <- arima(diff.ASC_niesezonowe.in,  # zmienna zależna
                  order = c(1, 0, 1))  # rzędy (p,d,q)) d=0 bo pierwsze różnice
arima111

coeftest(arima111)

# wartości AIC
AIC(arima111)
# wartosci BIC
BIC(arima111)


# test LR 
# sprawdzamy czy statystyka testowa jest w obszarze ktytycznym tak --> odrzucamy hipotezę 0 - prawastronna hi kwadrat
teststat<- 2*(as.numeric(logLik(arima312))-as.numeric(logLik(arima111)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )


# estymacji modelu ARIMA(1,1,0) dla diff.ASC_niesezonowe.in

arima110 <- arima(diff.ASC_niesezonowe.in,  # zmienna zależna
                  order = c(1, 0, 0))  # rzędy (p,d,q)) d=0 bo pierwsze różnice
arima110

coeftest(arima110)

# wartości AIC
AIC(arima110)
# wartosci BIC
BIC(arima110)



# estymacji modelu ARIMA(0,1,1) dla diff.ASC_niesezonowe.in

arima011 <- arima(diff.ASC_niesezonowe.in,  # zmienna zależna
                  order = c(0, 0, 1))  # rzędy (p,d,q)) d=0 bo pierwsze różnice
arima011

coeftest(arima011)

# wartości AIC
AIC(arima011)
# wartosci BIC
BIC(arima011)


# estymacji modelu ARIMA(0,1,0) dla diff.ASC_niesezonowe.in

arima010 <- arima(diff.ASC_niesezonowe.in,  # zmienna zależna
                  order = c(0, 0, 0))  # rzędy (p,d,q)) d=0 bo pierwsze różnice
arima010

coeftest(arima010)

# wartości AIC
AIC(arima010)

# wartosci BIC
BIC(arima010)



# Czy reszty są białym szumem? dla wybranego modelu ARIMA(1,1,2)
par(mfrow = c(1, 2))
Acf(resid(arima112), lag.max = 24,
    ylim = c(-1, 1),
    xlim=c(1,24),
    lwd = 4, col = "red",
    na.action = na.pass)    # nie przerywaj, jeśli w danych brakujace wartości 
Pacf(resid(arima112), lag.max =24 ,
     lwd = 4, col = "red",
     na.action = na.pass) # nie przerywaj, jeśli w danych brakujace wartości 
# reszty są białym szumem




# czyste reszty, znajduja sie w przedziale ufnosci, brak znaczacych wypustek
par(mfrow = c(1, 1))

# Test Ljung-Boxa (do sensowego opóźnienia).
Box.test(resid(arima112), type = "Ljung-Box", lag = 24)
Box.test(resid(arima112), type = "Box-Pierce", lag = 24)
# p-values > 0.05, czyli reszty są białym szumem





###########################################################################
# IV. Diagnostyka                                                         #
###########################################################################

# 1.
# Estymacji modelu ARIMA(1,1,2)

arima112 <- arima(diff.ASC_niesezonowe.in,  # zmienna zależna
                  order = c(1, 0, 2)  # rzędy (p,d,q)
)
arima112

coeftest(arima112)


# wartości AIC
AIC(arima112)
# wartosci BIC
BIC(arima112)

# Czy reszty są białym szumem?
par(mfrow = c(1, 2))
Acf(resid(arima112), lag.max = 24,
    ylim = c(-1, 1),
    xlim=c(1,24),
    lwd = 4, col = "red")
Pacf(resid(arima112), lag.max =24 ,
     lwd = 4, col = "red")



par(mfrow = c(1, 1))
# Test Ljung-Boxa (do sensowego opóźnienia).
Box.test(resid(arima112), type = "Ljung-Box", lag = 24)

# Test Box-Pierce (do sensowego opóźnienia).
Box.test(resid(arima112), type = "Box-Pierce", lag = 24)

#testy: wszystko ok  KONIEC 


###########################################################################
# V. Prognoza                                                             #
###########################################################################


# Wersja 1

# prognozy dla lwpi - model ARIMA(1,1,2)
nobs <- length(ASC_niesezonowe.in)

# szacujemy model na próbie IN SAMPLE, d=1 pierwsza różnica żeby zienna była stacjonarna 
arima112 <- arima(as.numeric(ASC_niesezonowe.in), # zmienna zależna
                    order = c(1, 0, 2),  # rzędy (p,d,q)
                    xreg = 1:nobs,       # dodatkowe regresory - stala, uwzględnienie stałej 
                  #  fixed = c(NA, NA, 0, 0, NA, NA) # chcemy wyrzuci8ć 2 i 3 element w części MA 
)
summary(arima112)

forecast_arima112<- predict(arima112, n.ahead = 3,
                          newxreg = (nobs + 1) : (nobs + 3)) # prognozy dla dodatkowych regresorów, wyjątkowo 3 do przodu 
# obejrzyjmy wynik
forecast_arima112

# zbior zawiera 2 elementy:
# pred - prognozy
# se - błąd standardowy prognozy

#Rozkład reszt
hist(residuals(arima112))

library(tseries)
jarque.bera.test(residuals(arima112))
#  rozklad normalny reszt




forecast_arima112 = ts(data=forecast_arima112$pred, frequency = 12,             
                   start=c(2022,10), end=c(2022,12)) 

forecast_arima112

str(forecast_arima112)

# wykres prognozy
plot(ASC_niesezonowe.ts, main = "3-miesięczna prognoza dla kursu walutowego USD/PLN")
abline(v = 2022+9/12, lty = 2, col = "gray")
lines(forecast_arima112, col = "blue", lwd = 2)
lines(forecast_arima112 + 2 * forecast_se, col = "red", lty = 3)
lines(forecast_arima112 - 2 * forecast_se, col = "red", lty = 3)


# i raz jeszcze,w zbliżeniu
plot(ASC_niesezonowe.ts, main = "3-miesięczna prognoza dla kursu walutowego USD/PLN",
     xlim = c(2021, 2023), ylim = c(3, 5.5))
abline(v = 2022+9/12, lty = 2, col = "gray")
lines(forecast_arima112,col = "blue",  lwd = 2)
lines(forecast_arima112 + 2 * forecast_se, col = "red", lty = 3)
lines(forecast_arima112 - 2 * forecast_se, col = "red", lty = 3)



# łączymy prognozy z oryginalnym szeregiem
niesezonowy.forecast <- data.frame(data=forecast_pred,
                                   real =  window(ASC_niesezonowe.ts,
                                                  start = c(2022, 10))
)
niesezonowy.forecast

str(niesezonowy.forecast)


###########################################################################
# VI. JAKOSC PROGNOZY                                                     #
###########################################################################


niesezonowy.forecast$mae <- abs(as.numeric(niesezonowy.forecast$real) - niesezonowy.forecast$data)
niesezonowy.forecast$mse <- (as.numeric(niesezonowy.forecast$real) - niesezonowy.forecast$data) ^ 2
niesezonowy.forecast$mape <- abs((as.numeric(niesezonowy.forecast$real) - niesezonowy.forecast$data) /
                             as.numeric(niesezonowy.forecast$real))
niesezonowy.forecast$amape <- abs((as.numeric(niesezonowy.forecast$real) - niesezonowy.forecast$data) /
                              (as.numeric(niesezonowy.forecast$real) + niesezonowy.forecast$data))

str(niesezonowy.forecast[, 3:6])

colMeans(niesezonowy.forecast[, 3:6])

# aby zmienić 'naukowy' format liczb
# możemy skorzystać z opcji scipen
options(scipen = 5)
round(colMeans(ln.df.forecast[, 3:6]), 3)


#	Należy  porównać kilka modeli w okresie out-of-sample aby wybrać najlepszy 


