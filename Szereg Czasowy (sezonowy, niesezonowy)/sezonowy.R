########################## Analiza dla modelu sezonowego #################################
#############     konsumpcja gazów cieplarnianych we Włoszech    #########################
##########################################################################################




# import pliku CSV

library(foreign)

ASC_sezonowe <- read.csv ("dane/Gworek_ASC_sezonowe.csv", # nazwa pliku
                             header = TRUE,                      # czy obserwacje w pierwszym wierszu?
                             sep = ";")	                       # separator dziesiatny

is.ts(ASC_sezonowe)                           
ASC_sezonowe


ASC_sezonowe.ts <- ts(ASC_sezonowe$Value, start=c(2012,1),  freq = 12 ) 


class(ASC_sezonowe) 

is.ts(ASC_sezonowe.ts)  


#  Wizualizacja szeregu 

plot(ASC_sezonowe.ts,
     main = "Konsumpcja gazów cieplarnianych we Włoszech",
     xlab = "data",
     ylab = "TJ (GCV)")



# okresy in-sample i out-of-sample z użyciem funkcji window()
sezonowy.in <- 
  window(ASC_sezonowe.ts,
         end = c(2021, 12))

sezonowy.out <-    # 12 obserwacji out-of sample (jeden cykl)
  window(ASC_sezonowe.ts,
         start = c(2022, 01))


#######################  Model ekstrapolacyjny #################################

# addytywny model Holta-Wintersa
sezonowy.HWadd <- HoltWinters(sezonowy.in,
                            seasonal = "additive") #szereg in-sample
sezonowy.HWadd 
plot(sezonowy.HWadd )

sezonowy.HWadd.forecast <- predict(sezonowy.HWadd,
                                 n.ahead = 12,
                                 prediction.interval = TRUE) #fit = prognoza, dalej dolny i gorny przedzial ufnosci

#library(rcompanion)
#par(mfrow=c(1,2))

plot(ASC_sezonowe.ts) # porownanie prognozy z oryginalnym szeregiem 
lines(sezonowy.HWadd.forecast [, 1], col = "blue") # prognoza
lines(sezonowy.HWadd.forecast [, 2], col = "red", lty = 2) # dolna granica przedziału ufności
lines(sezonowy.HWadd.forecast [, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 2022, lty = 2) # dodajemy pionową linię referencyjną, gdzie zaczuna sie okres out-of-sample
title("Model addytywny 2012-2022")

plot(window(ASC_sezonowe.ts, start = c(2020, 12)))  # porownanie prognozy z oryginalnym szeregiem 
lines(sezonowy.HWadd.forecast[, 1], col = "blue") # prognoza
lines(sezonowy.HWadd.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności
lines(sezonowy.HWadd.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 2022, lty = 2) # dodajemy pionową linię referencyjną, gdzie zaczuna sie okres out-of-sample
title("Model addytywny 2020-2022")


# multiplikatywny model Holta-Wintersa

sezonowy.HWmult <- HoltWinters(sezonowy.in,
                             seasonal="multiplicative")
sezonowy.HWmult 

plot(sezonowy.HWmult)

sezonowy.HWmult.forecast <- predict(sezonowy.HWmult ,
                                  n.ahead = 12,
                                  prediction.interval = TRUE) #fit = prognoza, dalej dolny i gorny przedzial ufnosci

plot(ASC_sezonowe.ts) # porownanie prognozy z oryginalnym szeregiem 
lines(sezonowy.HWmult.forecast[, 1], col = "blue") # prognoza
lines(sezonowy.HWmult.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności
lines(sezonowy.HWmult.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 2022, lty = 2) # dodajemy pionową linię referencyjną, gdzie zaczuna sie okres out-of-sample
title("Model multiplikatywny 2012-2022")

plot(window(ASC_sezonowe.ts, start = c(2020, 12)))  # porownanie prognozy z oryginalnym szeregiem 
lines(sezonowy.HWmult.forecast[, 1], col = "blue") # prognoza
lines(sezonowy.HWmult.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności
lines(sezonowy.HWmult.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 2022, lty = 2) # dodajemy pionową linię referencyjną, gdzie zaczuna sie okres out-of-sample
title("Model multiplikatywny 2020-2022")



#  porównanie błędów ex-post 

sezonowy.HWadd$fitted[, 1]
sezonowy.HWmult$fitted[, 1]

sezonowy.HWadd.summary <- window(sezonowy.HWadd$fitted[, 1], end =c(2022, 12) , extend = TRUE)
sezonowy.HWmult.summary <- window(sezonowy.HWmult$fitted[, 1], end =c(2022, 12) , extend = TRUE)

# w miejsce braków danych na końcu wstawiamy prognozy 
window(sezonowy.HWadd.summary, start = c(2022, 1)) <- sezonowy.HWadd.forecast[, 1]
window(sezonowy.HWmult.summary, start = c(2022, 1)) <- sezonowy.HWmult.forecast[, 1]

sezonowy.HWadd.summary
sezonowy.HWmult.summary

# łączymy wszystkie kolumny razem 
sezonowy.summary <- ts.union(ASC_sezonowe.ts, sezonowy.HWadd.summary,sezonowy.HWmult.summary)
sezonowy.summary
library(xts)
sezonowy.summary = as.xts(sezonowy.summary)
ifelse(index(sezonowy.summary) < "2022-01", 0, 1)

sample_period<-
  ts(ifelse(index(niesezonowy.summary) < "2022-01", 0, 1), 
     start  =c(2012, 1), freq = 12)

names(sezonowy.summary)



# nazwy trochę uprościmy
names(sezonowy.summary)<-c("ASC","HWadd","HWmult")

# utworzymy też zmienną indeksującą czas (date)
sezonowy.summary$date <- index(ASC_sezonowe.ts)

sezonowy.summary$sample_period <- sample_period


#porównanie 
sezonowy.summary$sample_period <- sample_period

sezonowy.summary$mae_HWadd <- abs(sezonowy.summary$HWadd-sezonowy.summary$ASC)
sezonowy.summary$mse_HWadd   <-(sezonowy.summary$HWadd-sezonowy.summary$ASC)^2
sezonowy.summary$mape_HWadd   <- abs((sezonowy.summary$HWadd-sezonowy.summary$ASC)/sezonowy.summary$ASC)
sezonowy.summary$amape_HWadd   <- abs((sezonowy.summary$HWadd-sezonowy.summary$ASC)/(sezonowy.summary$HWadd+sezonowy.summary$ASC))

sezonowy.summary$mae_HWmult   <- abs(sezonowy.summary$HWmult-sezonowy.summary$ASC)
sezonowy.summary$mse_HWmult      <- (sezonowy.summary$HWmult-sezonowy.summary$ASC)^2
sezonowy.summary$mape_HWmult     <- abs((sezonowy.summary$HWmult-sezonowy.summary$ASC)/sezonowy.summary$ASC)
sezonowy.summary$amape_HWmult    <- abs((sezonowy.summary$HWmult-sezonowy.summary$ASC)/(sezonowy.summary$HWmult+sezonowy.summary$ASC))


sezonowy.summary
aggregate(sezonowy.summary[, 6:13],
          by = list(sezonowy.summary$sample_period),
          FUN = function(x) mean(x, na.rm = T))

par(mfrow=c(1,1))


##############################################################################
######## powtarzam wszystyko dla zmiennej LOG bo za duże wartości ############
##############################################################################

###DANE####

# okresy in-sample i out-of-sample z użyciem funkcji window()

# 4. logarytmujemy szereg ma to na celu zminiejszenie wariancji
ln.sezonowy <- log(ASC_sezonowe.ts)




ln.sezonowy.in <- 
  window(ln.sezonowy,
         end = c(2021, 12))


ln.sezonowy.out <-    # 12 obserwacji out-of sample (jeden cykl)
  window(ln.sezonowy,
         start = c(2022, 01))




# wykresy szeregu wejsciowego i pierwszych roznic

# wykresy szeregu wejsciowego
par(mfrow=c(1,1))
plot(ln.sezonowy)



#  Wizualizacja szeregu 

plot(ln.sezonowy,
     main = "Konsumpcja gazów cieplarnianych we Włoszech",
     xlab = "data",
     ylab = "TJ (GCV)")

# Model ekstrapolacyjny


# addytywny model Holta-Wintersa
ln.sezonowy.HWadd <- HoltWinters(ln.sezonowy.in,
                              seasonal = "additive") #szereg in-sample
ln.sezonowy.HWadd 
plot(ln.sezonowy.HWadd )

ln.sezonowy.HWadd.forecast <- predict(ln.sezonowy.HWadd,
                                   n.ahead = 12,
                                   prediction.interval = TRUE) #fit = prognoza, dalej dolny i gorny przedzial ufnosci

#library(rcompanion)
par(mfrow=c(1,2))

plot(ln.sezonowy) # porownanie prognozy z oryginalnym szeregiem 
lines(ln.sezonowy.HWadd.forecast [, 1], col = "blue") # prognoza
lines(ln.sezonowy.HWadd.forecast [, 2], col = "red", lty = 2) # dolna granica przedziału ufności
lines(ln.sezonowy.HWadd.forecast [, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 2022, lty = 2) # dodajemy pionową linię referencyjną, gdzie zaczuna sie okres out-of-sample
title("Model addytywny 2012-2022")

plot(window(ln.sezonowy, start = c(2020, 12)))  # porownanie prognozy z oryginalnym szeregiem 
lines(ln.sezonowy.HWadd.forecast[, 1], col = "blue") # prognoza
lines(ln.sezonowy.HWadd.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności
lines(ln.sezonowy.HWadd.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 2022, lty = 2) # dodajemy pionową linię referencyjną, gdzie zaczuna sie okres out-of-sample
title("Model addytywny 2020-2022")


# multiplikatywny model Holta-Wintersa

ln.sezonowy.HWmult <- HoltWinters(ln.sezonowy.in,
                               seasonal="multiplicative")
ln.sezonowy.HWmult 

plot(ln.sezonowy.HWmult)

ln.sezonowy.HWmult.forecast <- predict(ln.sezonowy.HWmult ,
                                    n.ahead = 12,
                                    prediction.interval = TRUE) #fit = prognoza, dalej dolny i gorny przedzial ufnosci
ln.sezonowy.HWmult.forecast
plot(ln.sezonowy) # porownanie prognozy z oryginalnym szeregiem 
lines(ln.sezonowy.HWmult.forecast[, 1], col = "blue") # prognoza
lines(ln.sezonowy.HWmult.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności
#lines(ln.sezonowy.HWmult.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 2022, lty = 2) # dodajemy pionową linię referencyjną, gdzie zaczuna sie okres out-of-sample
title("Model multiplikatywny 2012-2022")

plot(window(ln.sezonowy, start = c(2020, 12)))  # porownanie prognozy z oryginalnym szeregiem 
lines(ln.sezonowy.HWmult.forecast[, 1], col = "blue") # prognoza
lines(ln.sezonowy.HWmult.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności
#lines(ln.sezonowy.HWmult.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 2022, lty = 2) # dodajemy pionową linię referencyjną, gdzie zaczuna sie okres out-of-sample
title("Model multiplikatywny 2020-2022")



#porównanie błędów ex-post 

ln.sezonowy.HWadd$fitted[, 1]
ln.sezonowy.HWmult$fitted[, 1]

ln.sezonowy.HWadd.summary <- window(ln.sezonowy.HWadd$fitted[, 1], end =c(2022, 12) , extend = TRUE)
ln.sezonowy.HWmult.summary <- window(ln.sezonowy.HWmult$fitted[, 1], end =c(2022, 12) , extend = TRUE)

# w miejsce braków danych na końcu wstawiamy prognozy 
window(ln.sezonowy.HWadd.summary, start = c(2022, 1)) <- ln.sezonowy.HWadd.forecast[, 1]
window(ln.sezonowy.HWmult.summary, start = c(2022, 1)) <- ln.sezonowy.HWmult.forecast[, 1]

ln.sezonowy.HWadd.summary
ln.sezonowy.HWmult.summary

# łączymy wszystkie kolumny razem 
ln.sezonowy.summary <- ts.union(ln.sezonowy, ln.sezonowy.HWadd.summary,ln.sezonowy.HWmult.summary)
ln.sezonowy.summary
library(xts)
ln.sezonowy.summary = as.xts(ln.sezonowy.summary)
ifelse(index(ln.sezonowy.summary) < "2022-01", 0, 1)

sample_period<-
  ts(ifelse(index(ln.sezonowy.summary) < "2022-01", 0, 1), 
     start  =c(2012, 1), freq = 12)

names(ln.sezonowy.summary)



# nazwy trochę uprościmy
names(ln.sezonowy.summary)<-c("ASC","HWadd","HWmult")

# utworzymy też zmienną indeksującą czas (date)
ln.sezonowy.summary$date <- index(ln.sezonowy)

ln.sezonowy.summary$sample_period <- sample_period


#porównanie 
ln.sezonowy.summary$sample_period <- sample_period

ln.sezonowy.summary$mae_HWadd <- abs(ln.sezonowy.summary$HWadd-ln.sezonowy.summary$ASC)
ln.sezonowy.summary$mse_HWadd   <-(ln.sezonowy.summary$HWadd-ln.sezonowy.summary$ASC)^2
ln.sezonowy.summary$mape_HWadd   <- abs((ln.sezonowy.summary$HWadd-ln.sezonowy.summary$ASC)/ln.sezonowy.summary$ASC)
ln.sezonowy.summary$amape_HWadd   <- abs((ln.sezonowy.summary$HWadd-ln.sezonowy.summary$ASC)/(ln.sezonowy.summary$HWadd+ln.sezonowy.summary$ASC))

ln.sezonowy.summary$mae_HWmult   <- abs(ln.sezonowy.summary$HWmult-ln.sezonowy.summary$ASC)
ln.sezonowy.summary$mse_HWmult      <- (ln.sezonowy.summary$HWmult-ln.sezonowy.summary$ASC)^2
ln.sezonowy.summary$mape_HWmult     <- abs((ln.sezonowy.summary$HWmult-ln.sezonowy.summary$ASC)/ln.sezonowy.summary$ASC)
ln.sezonowy.summary$amape_HWmult    <- abs((ln.sezonowy.summary$HWmult-ln.sezonowy.summary$ASC)/(ln.sezonowy.summary$HWmult+ln.sezonowy.summary$ASC))


ln.sezonowy.summary
aggregate(ln.sezonowy.summary[, 6:13],
          by = list(ln.sezonowy.summary$sample_period),
          FUN = function(x) mean(x, na.rm = T))

par(mfrow=c(1,1))

#####################################################################################################


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
source("funs/testdf.R")


###########################################################################
# Model SARIMA(p,d,q)(P,D,Q)                                              #
###########################################################################


###########################################################################
# I. Przygogowanie danych                                                 #
###########################################################################

# używamy tych samych danych co do modelu ekstrapolacyjny


# okresy in-sample i out-of-sample z użyciem funkcji window()
sezonowy.in <- 
  window(ASC_sezonowe.ts,
         end = c(2021, 12))


sezonowy.out <-    # 12 obserwacji out-of sample (jeden cykl)
  window(ASC_sezonowe.ts,
         start = c(2022, 01))




# wykresy szeregu wejsciowego i pierwszych roznic

# wykresy szeregu wejsciowego
plot(sezonowy.in)
# widoczna sezonowść alr eaczej brak trendu 

# wprowadzenie pierwszej roznicy regularnej
diff.sezonowy.in <- diff.xts(sezonowy.in, lag = 1)
plot(diff.sezonowy.in)

# logarytmujemy szereg ma to na celu zminiejszenie wariancji
ln.sezonowy.in <- log(sezonowy.in)



# Obejrzyjmy korelogramy dla ln.sezonowy.in
par(mar = rep(2, 4))

par(mfrow = c(2, 1))
Acf (ln.sezonowy.in, lag.max=36,
    lwd = 4,
    col = "red", na.action = na.pass)
Pacf (ln.sezonowy.in, lag.max=36,
     lwd = 4,
     col = "red", na.action = na.pass)
par(mfrow = c(1, 1))

# mamy wypstki sezonowe na 36 istotne, wygasają ale nie zbiegają do 0
# 12 wypsutka istotna 24 i 36 jest nieistotny 
# dla acf nie znikają  wypsutki więc raczje szereg niestacjonarny 


# wprowadzenie roznic regularnych dla log szeregu 

ln.diff.sezonowy.in <- diff.xts(ln.sezonowy.in, lag = 1)
ln.diff.sezonowy.in
plot(ln.diff.sezonowy.in,
     main = "Pierwsze różnice regularne dla logarytmu konsumpcji gazów cieplarnych we Włoszech")


# Obejrzyjmy korelogramy dla pierwszych różnic reguarnych 

par(mfrow = c(2, 1))
Acf(ln.diff.sezonowy.in,  lag.max = 36,
    xlim=c(2,36),
    lwd = 4,
    col = "red",
    main = "ACF",
na.action = na.pass)
Pacf(ln.diff.sezonowy.in, lag.max=36,
     lwd = 4,
     col = "red", na.action = na.pass)
par(mfrow = c(1, 1))


# wyidać że szereg jest niestacjnoarny sezonowy --> należy użyć różnic sezonowych 
# mamy tylko jedną wypsutke sezonową istotną statystycznie  
# PACF wskazuje na występowanoe 36 wypuski która nie wygasa, bo 24 istotna  


# 6. wprowadzenie roznic sezonowych

sdiff.ln.diff.sezonowy.in<- diff.xts(ln.diff.sezonowy.in, lag = 12)
plot(sdiff.ln.diff.sezonowy.in)

# Obejrzyjmy korelogramy dla roznic sezonowych
par(mfrow = c(2, 1))
Acf(sdiff.ln.diff.sezonowy.in,  lag.max = 36,
    xlim=c(2,36),
    lwd = 4,
    col = "red", na.action = na.pass)
Pacf(sdiff.ln.diff.sezonowy.in, lag.max=36,
     lwd = 4,
     col = "red", na.action = na.pass)
par(mfrow = c(1, 1))


# Sprawdzenie stacjonarności 
# Krok 1. Test Dickeya, Haszy, Fullera (DHF) dla log szeregu 

d12lse<- diff.xts(sdiff.ln.diff.sezonowy.in, lag = 12)
lag12lse<- lag.xts(sdiff.ln.diff.sezonowy.in, k = 12)


plot(d12lse)

model1=lm(d12lse~0+lag12lse)
summary(model1)
bg1 <- bgtest(model1, order = 1)
bg1

#brak autokorelacja, spr dalej 
bg2 <- bgtest(model1, order = 2)
bg2

#jest autokorelacja, dodajmy 1 rozszerzenie i spr czy nam to pozwoli na usuniecie autkorelacji reszt


 
lagd12lse <- lag.xts(sdiff.ln.diff.sezonowy.in, k = 1)
model2=lm(d12lse~0+lag12lse + lagd12lse)


summary(model2)
bg1 <- bgtest(model2, order = 1)
bg1
bg2 <- bgtest(model2, order = 2)
bg2

#autokorelacja 



lag2d12lse<- lag.xts(d12lse, k = 2)

model3=lm(d12lse~0+lag12lse+lagd12lse+lag2d12lse)
summary(model3)
bg1 <- bgtest(model3, order = 1)
bg1
bg2 <- bgtest(model3, order = 2)
bg2
bg3 <- bgtest(model3, order = 3)
bg3
bg4 <- bgtest(model3, order = 4)
bg4
bg5 <- bgtest(model3, order = 5)
bg5
bg6 <- bgtest(model3, order = 6)
bg6



bg1
bg2
bg3
bg4
bg5
bg6


# spełnione na poziomie 1% --> brak autokorelacji reszt 


# Podsumowując test ADHF (dla pierwszych różnic reguralnych i pierwszych różnic sezonowych):
# Statystyka testowa:   -18.836
# Statystyka krytyczna: -5.86
# Decyzja: Statystyka testowa <Statystyka krytyczna test lewostronny odrzucamy H0
#szereg jest stacjnarny 

#sprawdzamy tym testem wbudowanym 


testdf(variable = ln.diff.sezonowy.in ,ADF_type="nc", ADF_max_order = 3, BG_max_order = 6)
# dla pierwszych różnic reguralnych mamy autokorelacje reszt 

testdf(variable = sdiff.ln.diff.sezonowy.in ,ADF_type="nc", ADF_max_order = 3, BG_max_order = 6)
# brak utokorelacji reszt dla pierwszychn różnic reguralnych i sezonowych z dwoma opóźnieniami


#kpss test 
sdiff.ln.diff.sezonowy.in.kpss.test <- ur.kpss(sdiff.ln.diff.sezonowy.in, type = c("mu"))  # stała w równaniu testowym
summary(sdiff.ln.diff.sezonowy.in.kpss.test)
# statystyka testowa = 0.0268   < 0,463 => brak podstaw do odrzucenia h0 o stacjonarnosci sezonowego


# podsumowanie: otrzymaliśmy zatem szereg stacjonarny.
# d=1, D=1



# Czy zmienna  jest bialym szumem?

# H0: zmienna  jest bialym szumem

# Test Ljung-Boxa
Box.test(sdiff.ln.diff.sezonowy.in, type = "Ljung-Box", lag = 36)

# Test Boxa-Pierce
Box.test(sdiff.ln.diff.sezonowy.in, type = "Box-Pierce", lag = 36)

### testy wskazują, że zmienna jest białym szumem, ale testy mogą dawać błędny wynik --> dalsza analiza

# ACF i PACF
par(mfrow = c(2, 1))
Acf(sdiff.ln.diff.sezonowy.in,  lag.max = 36, 
    xlim=c(2,36),
    lwd = 4,
    col = "red", na.action = na.pass)
Pacf(sdiff.ln.diff.sezonowy.in, lag.max=36,
     xlim=c(2,36),
     lwd = 4,
     col = "red", na.action = na.pass)
par(mfrow = c(1, 1))

#  zmienn nie jest białym szumem, bo instnieją wyspustki któRe są istotne statystycznie




###########################################################################
# II. Identyfikacja                                                       #
###########################################################################

# IDENTYFIKACJA rzędów P i Q

# Analiza korelogramow ACF i PACF dla szeregu 



# ACF i PACF
par(mfrow = c(2, 1))
Acf(sdiff.ln.diff.sezonowy.in,  lag.max = 36, 
    xlim=c(2,36),
    lwd = 4,
    col = "red", na.action = na.pass)
Pacf(sdiff.ln.diff.sezonowy.in, lag.max=36,
     xlim=c(2,36),
     lwd = 4,
     col = "red", na.action = na.pass)
par(mfrow = c(1, 1))


# Korelogramy sugerują, że możemy mieć do czynienia z sezonowym procesem MA
# (malejąca PACF dla wielokrotnośći 12 opóźnienia) możemy przypuszczać, że SAR(1) i SMA(1)

###########################################################################
# III. Estymacja                                                          #
###########################################################################
nobs <- length(ln.sezonowy.in)

# SARIMA(0,1,0)(1,1,1)
arima010111 <- arima(ln.sezonowy.in,
                     # rzędy (p,d,q)
                     order = c(0, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(1, 1, 1),
                                     # częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12),
                     xreg = 1:nobs       # dodatkowe regresory - stala
)





arima010111
coeftest(arima010111)
# Oba parametry sa istotne statystycznie na poziomie 5%, stala nie jest istotna statystycznie
#aby sprawdzic pozostale parametry metodą od ogółu do szzegółu robimy drzewko 

par(mfrow = c(2, 1))
Acf(resid(arima010111), lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36), lwd = 4, col = "red")
Pacf(resid(arima010111), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

# Estymacji modelu SARIMA(0,1,0)(0,1,1)

arima010011 <- arima(ln.sezonowy.in,
                     # rzędy (p,d,q)
                     order = c(0, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),
                                     # częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12),
                     xreg = 1:nobs       # dodatkowe regresory - stala
)

arima010011
coeftest(arima010011)
# parametr przy sezonowym efekcie SMA jest istotny

par(mfrow = c(2, 1))
Acf(resid(arima010011), lag.max = 36,
    ylim = c(-0.4, 0.4),xlim=c(2,36), lwd = 4, col = "red")
Pacf(resid(arima010011), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

# test LR
teststat<- 2*(as.numeric(logLik(arima010111))-as.numeric(logLik(arima010011)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )
# na pozimmoe 5%  SARIMA(0,1,0)(1,1,1) lepszy od SARIMA(0,1,0)(0,1,1)

# Estymacji modelu SARIMA(0,1,0)(0,1,0)

arima010010 <- arima(ln.sezonowy.in,
                     # rzędy (p,d,q)
                     order = c(0, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 0),
                                     # częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12),
                     xreg = 1:nobs       # dodatkowe regresory - stala
)

arima010010
coeftest(arima010010)


# test LR
teststat<- 2*(as.numeric(logLik(arima010111))-as.numeric(logLik(arima010010)))
teststat

pchisq(teststat, df=2, lower.tail = FALSE )


# wartości AIC
AIC(arima010111, arima010011, arima010010)
#czesc sezonowa SARIMA(0,1,0)(0,1,1) - AIC
# wartosci BIC
BIC(arima010111, arima010011, arima010010) 
#czesc sezonowa SARIMA(0,1,0)(0,1,1) - BIC

#rozważamy dalej dwa modele arima010111 oraz arima010011


# ACF PACF arima010011
par(mfrow = c(2, 1))
Acf(resid(arima010011), lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36), lwd = 4, col = "red")
 title("SARIMA (0,1,0)(0,1,1)")
Pacf(resid(arima010011), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

# ACF PACF arima010111
par(mfrow = c(2, 1))
Acf(resid(arima010111), lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36), lwd = 4, col = "red")
title("SARIMA (0,1,0)(1,1,1)")
Pacf(resid(arima010111), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))




# efekty sezonowe zostały wyjaśnione
# przystępujemy do identyfikacji efektóW regularnych

# SARIMA(5,1,1)(1,1,1)
arima511111 <- arima(ln.sezonowy.in,
                     order = c(5, 1, 1),
                     seasonal = list(order = c(1, 1, 1),
                                     period = 12)
)
arima511111
coeftest(arima511111)

par(mfrow = c(2, 1))
Acf(resid(arima511111),  lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36),
    lwd = 4, col = "red")
Pacf(resid(arima511111), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

# czy reszty są białym szumem?
# test Ljung-Boxa
Box.test(resid(arima511111), type = "Ljung-Box", lag = 36)
Box.test(resid(arima511111), type = "Box-Pierce", lag = 36)


# reszty wydają się być białym szumem


# SARIMA(4,1,1)(1,1,1)
arima411111 <- arima(ln.sezonowy.in,
                     order = c(4, 1, 1),
                     seasonal = list(order = c(1, 1, 1),
                                     period = 12)
)
arima411111
coeftest(arima411111)

par(mfrow = c(2, 1))
Acf(resid(arima411111),  lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36),
    lwd = 4, col = "red")
Pacf(resid(arima411111), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

# czy reszty są białym szumem?
# test Ljung-Boxa
Box.test(resid(arima411111), type = "Ljung-Box", lag = 36)
Box.test(resid(arima411111), type = "Box-Pierce", lag = 36)



# reszty nie są białym szumem

# test LR
teststat<- 2*(as.numeric(logLik(arima511111))-as.numeric(logLik(arima411111)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )



# SARIMA(3,1,1)(1,1,1)
arima311111 <- arima(ln.sezonowy.in,
                     order = c(3, 1, 1),
                     seasonal = list(order = c(1, 1, 1),
                                     period = 12)
)
arima311111
coeftest(arima311111)

par(mfrow = c(2, 1))
Acf(resid(arima311111),  lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36),
    lwd = 4, col = "red")
Pacf(resid(arima311111), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

# czy reszty są białym szumem?
# test Ljung-Boxa
Box.test(resid(arima311111), type = "Ljung-Box", lag = 36)
Box.test(resid(arima311111), type = "Box-Pierce", lag = 36)



# reszty nie są białym szumem



# test LR
teststat<- 2*(as.numeric(logLik(arima511111))-as.numeric(logLik(arima311111)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )



# SARIMA(2,1,1)(1,1,1)
arima211111 <- arima(ln.sezonowy.in,
                     order = c(2, 1, 1),
                     seasonal = list(order = c(1, 1, 1),
                                     period = 12)
)
arima211111
coeftest(arima211111)

par(mfrow = c(2, 1))
Acf(resid(arima211111),  lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36),
    lwd = 4, col = "red")
Pacf(resid(arima211111), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

# czy reszty są białym szumem?
# test Ljung-Boxa
Box.test(resid(arima211111), type = "Ljung-Box", lag = 36)
Box.test(resid(arima211111), type = "Box-Pierce", lag = 36)



# reszty nie są białym szumem


# test LR
teststat<- 2*(as.numeric(logLik(arima511111))-as.numeric(logLik(arima211111)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )




# SARIMA(1,1,1)(1,1,1)
arima111111 <- arima(ln.sezonowy.in,
                     order = c(1, 1, 1),
                     seasonal = list(order = c(1, 1, 1),
                                     period = 12)
)
arima111111
coeftest(arima111111)

par(mfrow = c(2, 1))
Acf(resid(arima111111),  lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36),
    lwd = 4, col = "red")
Pacf(resid(arima111111), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

# czy reszty są białym szumem?
# test Ljung-Boxa
Box.test(resid(arima111111), type = "Ljung-Box", lag = 36)
Box.test(resid(arima111111), type = "Box-Pierce", lag = 36)



# reszty nie są białym szumem


# test LR
teststat<- 2*(as.numeric(logLik(arima511111))-as.numeric(logLik(arima111111)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )


# SARIMA(0,1,1)(1,1,1)
arima011111 <- arima(ln.sezonowy.in,
                     order = c(0, 1, 1),
                     seasonal = list(order = c(1, 1, 1),
                                     period = 12)
)
arima011111
coeftest(arima011111)

par(mfrow = c(2, 1))
Acf(resid(arima011111),  lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36),
    lwd = 4, col = "red")
Pacf(resid(arima011111), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

# czy reszty są białym szumem?
# test Ljung-Boxa
Box.test(resid(arima011111), type = "Ljung-Box", lag = 36)
Box.test(resid(arima011111), type = "Box-Pierce", lag = 36)

# reszty nie są białym szumem


# test LR
teststat<- 2*(as.numeric(logLik(arima511111))-as.numeric(logLik(arima011111)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )



# SARIMA(1,1,0)(1,1,1)
arima110111 <- arima(ln.sezonowy.in,
                     order = c(1, 1, 0),
                     seasonal = list(order = c(1, 1, 1),
                                     period = 12)
)
arima110111
coeftest(arima110111)

par(mfrow = c(2, 1))
Acf(resid(arima110111),  lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36),
    lwd = 4, col = "red")
Pacf(resid(arima110111), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

# czy reszty są białym szumem?
# test Ljung-Boxa
Box.test(resid(arima110111), type = "Ljung-Box", lag = 36)
Box.test(resid(arima110111), type = "Box-Pierce", lag = 36)

# reszty nie są białym szumem



# test LR
teststat<- 2*(as.numeric(logLik(arima511111))-as.numeric(logLik(arima110110)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )


# wartości AIC
AIC(arima511111, arima411111, arima311111, arima211111, arima111111, arima011111, arima110111)
#czesc sezonowa SARIMA(0,1,0)(0,1,1) - AIC
# wartosci BIC
BIC (arima511111, arima411111, arima311111, arima211111, arima111111, arima011111, arima110111) 
#czesc sezonowa SARIMA(0,1,0)(0,1,1) - BIC




# SARIMA(p,d,q)(0,1,1)



# efekty sezonowe zostały wyjaśnione
# przystępujemy do identyfikacji efektóW regularnych 

# SARIMA(3,1,1)(0,1,1)
arima311011 <- arima(ln.sezonowy.in,
                     order = c(3, 1, 1),
                     seasonal = list(order = c(0, 1, 1),
                                     period = 12)
)
arima311011
coeftest(arima311011)

par(mfrow = c(2, 1))
Acf(resid(arima311011),  lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36),
    lwd = 4, col = "red")
Pacf(resid(arima311011), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

# czy reszty są białym szumem?
# test Ljung-Boxa
Box.test(resid(arima311011), type = "Ljung-Box", lag = 36)
Box.test(resid(arima311011), type = "Box-Pierce", lag = 36)


# reszty wydają się być białym szumem


# SARIMA(2,1,1)(0,1,1)
arima211011 <- arima(ln.sezonowy.in,
                     order = c(2, 1, 1),
                     seasonal = list(order = c(0, 1, 1),
                                     period = 12)
)
arima211011
coeftest(arima211011)

par(mfrow = c(2, 1))
Acf(resid(arima211011),  lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36),
    lwd = 4, col = "red")
Pacf(resid(arima211011), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

# czy reszty są białym szumem?
# test Ljung-Boxa
Box.test(resid(arima211011), type = "Ljung-Box", lag = 36)
Box.test(resid(arima211011), type = "Box-Pierce", lag = 36)



# reszty nie są białym szumem

# test LR
teststat<- 2*(as.numeric(logLik(arima311011))-as.numeric(logLik(arima211011)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )



# SARIMA(1,1,1)(0,1,1)
arima111011 <- arima(ln.sezonowy.in,
                     order = c(1, 1, 1),
                     seasonal = list(order = c(0, 1, 1),
                                     period = 12)
)
arima111011
coeftest(arima111011)

par(mfrow = c(2, 1))
Acf(resid(arima111011),  lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36),
    lwd = 4, col = "red")
Pacf(resid(arima111011), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

# czy reszty są białym szumem?
# test Ljung-Boxa oraz Box-Pierce
Box.test(resid(arima111011), type = "Ljung-Box", lag = 36)
Box.test(resid(arima111011), type = "Box-Pierce", lag = 36)

# reszty nie są białym szumem


# test LR
teststat<- 2*(as.numeric(logLik(arima311011))-as.numeric(logLik(arima111011)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )


# SARIMA(0,1,1)(0,1,1)
arima011011 <- arima(ln.sezonowy.in,
                     order = c(0, 1, 1),
                     seasonal = list(order = c(0, 1, 1),
                                     period = 12)
)
arima011011
coeftest(arima011011)

par(mfrow = c(2, 1))
Acf(resid(arima011011),  lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36),
    lwd = 4, col = "red")
Pacf(resid(arima011011), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

# czy reszty są białym szumem?
# test Ljung-Boxa
Box.test(resid(arima011011), type = "Ljung-Box", lag = 36)
Box.test(resid(arima011011), type = "Box-Pierce", lag = 36)
# reszty nie są białym szumem



# test LR
teststat<- 2*(as.numeric(logLik(arima311011))-as.numeric(logLik(arima011011)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )



# SARIMA(0,1,0)(0,1,1)
arima010011 <- arima(ln.sezonowy.in,
                     order = c(0, 1, 0),
                     seasonal = list(order = c(0, 1, 1),
                                     period = 12)
)
arima010011
coeftest(arima010011)

par(mfrow = c(2, 1))
Acf(resid(arima010011),  lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36),
    lwd = 4, col = "red")
Pacf(resid(arima010011), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

# czy reszty są białym szumem?
# test Ljung-Boxa
Box.test(resid(arima010011), type = "Ljung-Box", lag = 36)
Box.test(resid(arima010011), type = "Box-Pierce", lag = 36)



# reszty nie są białym szumem# reszty nie są białym szumem

# test LR
teststat<- 2*(as.numeric(logLik(arima311011))-as.numeric(logLik(arima010011)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )


#najlepszy SARIMA(0,1,1)(0,1,1)


# wartości AIC
AIC(arima311011, arima211011, arima111011, arima011011, arima010011)
#czesc sezonowa SARIMA(1,1,1)(0,1,1) - AIC
# wartosci BIC
BIC(arima311011, arima211011, arima111011, arima011011, arima010011)
#czesc sezonowa SARIMA(0,1,1)(0,1,1) - BIC

# reszty nie są białym szumem

# test LR
teststat<- 2*(as.numeric(logLik(arima311011))-as.numeric(logLik(arima010011)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )

# reszty wydają się być białym szumem --> może to znaczyć ż model nie będzie dobrze prognozował



###########################################################################
# V. Prognoza                                                             #
###########################################################################



ln.sezonowy<- ts(log(ASC_sezonowe$Value), start=c(2012,1),  freq = 12 )
ln.sezonowy

# SARIMA(0,1,1)(0,1,1)
arima011011 <- arima(ln.sezonowy,
                     order = c(0, 1, 1),
                     seasonal = list(order = c(0, 1, 1),
                                     period = 12)
)

arima011011
forecast_arima011011 <- predict(arima011011, n.ahead = 12)

# obejrzyjmy wyniki
forecast_arima011011
str(forecast_arima011011)

# zbior zawiera 2 elementy:
# pred - prognozy
# se - błąd standardowy prognozy

#forecast_arima011011

forecast_pred_1 = ts(data=forecast_arima011011$pred, frequency = 12,             
                   start=c(2022,1), end=c(2022,12)) 

forecast_pred_1

forecast_se_1 = ts(data=forecast_arima011011$se, frequency = 12,             
                 start=c(2022,1), end=c(2022,12)) 

forecast_se_1

# wykres prognozy
plot(ln.sezonowy, main = "Konsumpcja Gazów Cieplarnianych we Włoszech")
abline(v = 2022, lty = 2, col = "gray")
lines(forecast_pred_1, col = "red", lwd = 2)
lines(forecast_pred_1 + 2.5 * forecast_se_1, col = "red", lty = 3)
lines(forecast_pred_1- 2.5 * forecast_se_1, col = "red", lty = 3)

# w zbliżeniu
plot(ln.sezonowy, main = "SARIMA(0,1,1)(0,1,1)",
 xlim = c(2021, 2023))

abline(v = 2022, lty = 2, col = "gray")
lines(forecast_pred_1, col = "red", lwd = 2)
lines(forecast_pred_1+ 2.5 * forecast_se_1, col = "red", lty = 3)
lines(forecast_pred_1 - 2.5 * forecast_se_1, col = "red", lty = 3)





# łączymy prognozy z oryginalnym szeregiem

forecast_arima011011<- data.frame(forecast = forecast_arima011011$pred,
                                  real = window(ln.sezonowy,
                                                start = c(2022,1))
)
forecast_arima011011

str(forecast_arima011011)




forecast_arima011011$mae <- abs(as.numeric(forecast_arima011011$real) - forecast_arima011011$forecast)
forecast_arima011011$mse <- (as.numeric(forecast_arima011011$real) - forecast_arima011011$forecast) ^ 2
forecast_arima011011$mape <- abs((as.numeric(forecast_arima011011$real) - forecast_arima011011$forecast) /
                                   as.numeric(forecast_arima011011$real))
forecast_arima011011$amape <- abs((as.numeric(forecast_arima011011$real) - forecast_arima011011$forecast) /
                                    (as.numeric(forecast_arima011011$real) + forecast_arima011011$forecast))
str(forecast_arima011011[, 3:6])

colMeans(forecast_arima011011[, 3:6])

# aby zmienić 'naukowy' format liczb
# możemy skorzystać z opcji scipen
options(scipen = 5)
round(colMeans(forecast_arima011011[, 3:6]), 3)





# SARIMA(1,1,1)(1,1,1)
arima111111 <- arima(ln.sezonowy,
                     order = c(1, 1, 1),
                     seasonal = list(order = c(1, 1, 1),
                                     period = 12)
)


forecast_arima111111 <- predict(arima111111, n.ahead = 12)

# obejrzyjmy wyniki
forecast_arima111111
str(forecast_arima111111)

# zbior zawiera 2 elementy:
# pred - prognozy
# se - błąd standardowy prognozy



forecast_pred_2 = ts(data=forecast_arima111111$pred, frequency = 12,             
                   start=c(2022,1), end=c(2022,12)) 

forecast_pred_2

forecast_se_2 = ts(data=forecast_arima111111$se, frequency = 12,             
                 start=c(2022,1), end=c(2022,12)) 

forecast_se_2

# wykres prognozy
plot(ln.sezonowy, main = "Konsumpcja Gazów Cieplarnianych we Włoszech")
abline(v = 2022, lty = 2, col = "gray")
lines(forecast_pred_2, col = "red", lwd = 2)
lines(forecast_pred_2 + 2.5 * forecast_se_2, col = "red", lty = 3)
lines(forecast_pred_2 - 2.5 * forecast_se_2, col = "red", lty = 3)


# w zbliżeniu
plot(ln.sezonowy, main = "SARIMA(1,1,1)(1,1,1)",
     xlim = c(2021, 2023))

abline(v = 2022, lty = 2, col = "gray")
lines(forecast_pred_2, col = "red", lwd = 2)
lines(forecast_pred_2 + 2.5 * forecast_se_2, col = "red", lty = 3)
lines(forecast_pred_2 - 2.5 * forecast_se_2, col = "red", lty = 3)





# łączymy prognozy z oryginalnym szeregiem

forecast_arima111111<- data.frame(forecast = forecast_arima111111$pred,
                                  real = window(ln.sezonowy,
                                                start = c(2022,1))
)
forecast_arima111111

str(forecast_arima111111)



forecast_arima111111$mae <- abs(as.numeric(forecast_arima111111$real) - forecast_arima111111$forecast)
forecast_arima111111$mse <- (as.numeric(forecast_arima111111$real) - forecast_arima111111$forecast) ^ 2
forecast_arima111111$mape <- abs((as.numeric(forecast_arima111111$real) - forecast_arima111111$forecast) /
                                   as.numeric(forecast_arima111111$real))
forecast_arima111111$amape <- abs((as.numeric(forecast_arima111111$real) - forecast_arima111111$forecast) /
                                    (as.numeric(forecast_arima111111$real) + forecast_arima111111$forecast))
str(forecast_arima111111[, 3:6])

colMeans(forecast_arima111111[, 3:6])

# aby zmienić 'naukowy' format liczb
# możemy skorzystać z opcji scipen
options(scipen = 5)
round(colMeans(forecast_arima111111[, 3:6]), 3)



# SARIMA(1,1,1)(0,1,1)
arima111011 <- arima(ln.sezonowy,
                     order = c(1, 1, 1),
                     seasonal = list(order = c(0, 1, 1),
                                     period = 12)
)


forecast_arima111011 <- predict(arima111011, n.ahead = 12)

# obejrzyjmy wyniki
forecast_arima111011
str(forecast_arima111011)

# zbior zawiera 2 elementy:
# pred - prognozy
# se - błąd standardowy prognozy



forecast_pred_3 = ts(data=forecast_arima111011$pred, frequency = 12,             
                     start=c(2022,1), end=c(2022,12)) 

forecast_pred_3

forecast_se_3 = ts(data=forecast_arima111011$se, frequency = 12,             
                   start=c(2022,1), end=c(2022,12)) 

forecast_se_3

# wykres prognozy
par(mfrow=c(1,2))
plot(ln.sezonowy, main = "Konsumpcja Gazów Cieplarnianych we Włoszech")
abline(v = 2022, lty = 2, col = "gray")
lines(forecast_pred_3, col = "blue", lwd = 2)
lines(forecast_pred_3 + 2 * forecast_se_3, col = "red", lty = 3)
lines(forecast_pred_3 - 2 * forecast_se_3, col = "red", lty = 3)


# w zbliżeniu
plot(ln.sezonowy, main = " SARIMA(1,1,1)(0,1,1)",
     xlim = c(2021, 2023))

abline(v = 2022, lty = 2, col = "gray")
lines(forecast_pred_3, col = "blue", lwd = 2)
lines(forecast_pred_3 + 2 * forecast_se_3, col = "red", lty = 3)
lines(forecast_pred_3 - 2 * forecast_se_3, col = "red", lty = 3)



# łączymy prognozy z oryginalnym szeregiem

forecast_arima111011<- data.frame(forecast = forecast_arima111011$pred,
                                  real = window(ln.sezonowy,
                                                start = c(2022,1))
)
forecast_arima111011

str(forecast_arima111011)



forecast_arima111011$mae <- abs(as.numeric(forecast_arima111011$real) - forecast_arima111011$forecast)
forecast_arima111011$mse <- (as.numeric(forecast_arima111011$real) - forecast_arima111011$forecast) ^ 2
forecast_arima111011$mape <- abs((as.numeric(forecast_arima111011$real) - forecast_arima111011$forecast) /
                                   as.numeric(forecast_arima111011$real))
forecast_arima111011$amape <- abs((as.numeric(forecast_arima111011$real) - forecast_arima111011$forecast) /
                                    (as.numeric(forecast_arima111011$real) + forecast_arima111011$forecast))
str(forecast_arima111011[, 3:6])

colMeans(forecast_arima111011[, 3:6])

# aby zmienić 'naukowy' format liczb
# możemy skorzystać z opcji scipen
options(scipen = 5)
round(colMeans(forecast_arima111011[, 3:6]), 3)




# SARIMA(0,1,1)(1,1,1)
arima011111 <- arima(ln.sezonowy,
                     order = c(0, 1, 1),
                     seasonal = list(order = c(1, 1, 1),
                                     period = 12)
)


forecast_arima011111 <- predict(arima011111, n.ahead = 12)

# obejrzyjmy wyniki
forecast_arima011111
str(forecast_arima011111)

# zbior zawiera 2 elementy:
# pred - prognozy
# se - błąd standardowy prognozy



forecast_pred_4 = ts(data=forecast_arima011111$pred, frequency = 12,             
                     start=c(2022,1), end=c(2022,12)) 

forecast_pred_4

forecast_se_4 = ts(data=forecast_arima011111$se, frequency = 12,             
                   start=c(2022,1), end=c(2022,12)) 

forecast_se_4

# wykres prognozy
par(mfrow=c(1,2))
plot(ln.sezonowy, main = "SARIMA(0,1,1)(1,1,1)")
abline(v = 2022, lty = 2, col = "gray")
lines(forecast_pred_4, col = "blue", lwd = 2)
lines(forecast_pred_4 + 2 * forecast_se_4, col = "red", lty = 3)
lines(forecast_pred_4 - 2 * forecast_se_4, col = "red", lty = 3)


# w zbliżeniu
plot(ln.sezonowy, main = "SARIMA(0,1,1)(1,1,1)",
     xlim = c(2021, 2023))

abline(v = 2022, lty = 2, col = "gray")
lines(forecast_pred_4, col = "blue", lwd = 2)
lines(forecast_pred_4 + 2 * forecast_se_4, col = "red", lty = 3)
lines(forecast_pred_4 - 2 * forecast_se_4, col = "red", lty = 3)





# łączymy prognozy z oryginalnym szeregiem

forecast_arima011111<- data.frame(forecast = forecast_arima011111$pred,
                                  real = window(ln.sezonowy,
                                                start = c(2022,1))
)
forecast_arima011111

str(forecast_arima011111)



forecast_arima011111$mae <- abs(as.numeric(forecast_arima011111$real) - forecast_arima011111$forecast)
forecast_arima011111$mse <- (as.numeric(forecast_arima011111$real) - forecast_arima011111$forecast) ^ 2
forecast_arima011111$mape <- abs((as.numeric(forecast_arima011111$real) - forecast_arima011111$forecast) /
                                   as.numeric(forecast_arima011111$real))
forecast_arima011111$amape <- abs((as.numeric(forecast_arima011111$real) - forecast_arima011111$forecast) /
                                    (as.numeric(forecast_arima011111$real) + forecast_arima011111$forecast))
str(forecast_arima011111[, 3:6])

colMeans(forecast_arima011111[, 3:6])

# aby zmienić 'naukowy' format liczb
# możemy skorzystać z opcji scipen
options(scipen = 5)
round(colMeans(forecast_arima011111[, 3:6]), 3)

#podsumowanie błędów dla SARIMA
round(colMeans(forecast_arima111111[, 3:6]), 3)
round(colMeans(forecast_arima011111[, 3:6]), 3)
round(colMeans(forecast_arima111011[, 3:6]), 3)
round(colMeans(forecast_arima011011[, 3:6]), 3)



#załącznik
# w zbliżeniu

par(mfrow=c(2,2))

plot(ln.sezonowy, main = "SARIMA(0,1,1)(0,1,1)",
     xlim = c(2021, 2023))

abline(v = 2022, lty = 2, col = "gray")
lines(forecast_pred_1, col = "blue", lwd = 2)
lines(forecast_pred_1+ 2 * forecast_se_1, col = "red", lty = 3)
lines(forecast_pred_1 - 2 * forecast_se_1, col = "red", lty = 3)





# w zbliżeniu
plot(ln.sezonowy, main = "SARIMA(1,1,1)(1,1,1)",
     xlim = c(2021, 2023))

abline(v = 2022, lty = 2, col = "gray")
lines(forecast_pred_2, col = "blue", lwd = 2)
lines(forecast_pred_2 + 2 * forecast_se_2, col = "red", lty = 3)
lines(forecast_pred_2 - 2 * forecast_se_2, col = "red", lty = 3)



# w zbliżeniu
plot(ln.sezonowy, main = " SARIMA(0,1,1)(1,1,1)",
     xlim = c(2021, 2023))

abline(v = 2022, lty = 2, col = "gray")
lines(forecast_pred_4, col = "blue", lwd = 2)
lines(forecast_pred_4 + 2 * forecast_se_4, col = "red", lty = 3)
lines(forecast_pred_4 - 2 * forecast_se_4, col = "red", lty = 3)




# w zbliżeniu
plot(ln.sezonowy, main = " SARIMA(1,1,1)(0,1,1)",
     xlim = c(2021, 2023))

abline(v = 2022, lty = 2, col = "gray")
lines(forecast_pred_3, col = "blue", lwd = 2)
lines(forecast_pred_3 + 2 * forecast_se_3, col = "red", lty = 3)
lines(forecast_pred_3 - 2 * forecast_se_3, col = "red", lty = 3)

