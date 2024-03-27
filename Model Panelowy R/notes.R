library(plm)  #określenie biblioteki 
library(foreign) #określenie drugiej biblioteki
dane_do_modelu <- read.csv("dane/dane_do_modelu.csv",    # nazwa pliku
                           header = TRUE,    	           # czy obserwacje w pierwszym wierszu? 
                           sep = ";",	                   # separator kolumn
                           dec = ",")                    # separator dziesiętny

class(dane_do_modelu) #obiekt typu "data frame"

dane_do_modelu_p <- pdata.frame(dane_do_modelu, index = c("country", "year"))  #żeby dane były jako panel czytane

model1 <- plm(gini ~ inflacja + I(inflacja^2) + wz_real_PKB + wyd_rzad_PKB + bezrobocie + globalizacja + technologia , data = dane_do_modelu_p, index = c("country", "year"), model = "within", effect="time")
summary(model1)


model2 <- plm(gini ~ inflacja + I(inflacja^2) + wz_real_PKB + wyd_rzad_PKB + globalizacja , data = dane_do_modelu_p, index = c("country", "year"), model = "within", effect="time")
summary(model2)


model3 <- plm(gini ~ inflacja + I(inflacja^2) + wz_real_PKB + wyd_rzad_PKB + globalizacja + technologia , data = dane_do_modelu_p, index = c("country", "year"), model = "within", effect="time")
summary(model3)

model_ols <- plm(formula= gini ~ inflacja + I(inflacja^2) + wz_real_PKB + wyd_rzad_PKB + globalizacja,
                 data = dane_do_modelu_p,
                 index= c("country", "year"),
                 model= "pooling")
summary(model_ols)

model_between <- plm(formula= gini ~ inflacja + I(inflacja^2) + wz_real_PKB + wyd_rzad_PKB + globalizacja,
                     data = dane_do_modelu_p,
                     index= c("country", "year"),
                     model= "between")
summary(model_between)

model_re <- plm(formula= gini ~ inflacja + I(inflacja^2) + wz_real_PKB + wyd_rzad_PKB + globalizacja,
                data = dane_do_modelu_p,
                index= c("country", "year"),
                model= "random",
                erandom.method= "walhus"
) 

summary(model_re)

model_fe <- plm(formula= gini ~inflacja + I(inflacja^2) + wz_real_PKB + wyd_rzad_PKB + globalizacja,
                data = dane_do_modelu_p,
                index= c("country", "year"),
                model= "within",
                effect="time"
)
summary(model_fe)

model_fe_tech <- plm(formula= gini ~inflacja + I(inflacja^2) + wz_real_PKB + wyd_rzad_PKB + globalizacja+technologia,
                data = dane_do_modelu_p,
                index= c("country", "year"),
                model= "within",
                effect="time"
)
summary(model_fe_tech)

model_fe2 <- plm(formula= gini ~inflacja + I(inflacja^4) + wz_real_PKB + wyd_rzad_PKB + globalizacja,
                data = dane_do_modelu_p,
                index= c("country", "year"),
                model= "within"
summary(model_fe2)

#Test Breusch-Pagan
library(lmtest)

bptest(model_re,studentize = FALSE)

#husman test
phtest(model_fe, model_re)

#Test Wooldridge'a 
pbgtest(model_ols, order = 1, type = "F")


# Load the car package
library(car)


# Create an influence plot
influencePlot(model_fe, id.method = "identify", main = "Influence Plot")




# Załaduj pakiet corrplot
library(corrplot)



# Oblicz macierz korelacji Pearsona
corr_matrix <- cor(dane_do_modelu_p[, 6:10], method = "pearson")

# Utwórz histogram korelacji
corrplot(corr_matrix, method = "color", type = "full")



modelt <- plm(gini ~ inflacja + I(inflacja^2) + wz_real_PKB + wyd_rzad_PKB  + globalizacja + technologia , data = dane_do_modelu_p, index = c("country", "year"), model = "within", effect="time")
summary(modelt)


model_fe_3 <- plm(formula= gini ~inflacja + I(inflacja^2), 
                     data = dane_do_modelu_p,
                     index= c("country", "year"),
                     model= "within",
                     effect="time"
)
summary(model_fe_3)

model1 <- plm(gini ~ inflacja + I(inflacja^2) + wz_real_PKB + wyd_rzad_PKB + bezrobocie + globalizacja + technologia , data = dane_do_modelu_p, index = c("country", "year"), model = "within",)
summary(model1)

summary(model_fe)

fpc(model_fe, type = "Chow")
