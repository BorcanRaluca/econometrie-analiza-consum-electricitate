setwd('D:/facultate/cursuri/anul_3/sem1/econometrie/proiect/cod/electricity')

rm(list = ls()) 

energy <- read.csv("energy_romania7.csv")
View(energy)
energy %<>% select(consumption_electricity, consumption_natural_gas, imports_natural_gas,
                   production_electricity, imports_electricity, production_natural_gas,
                   imports_natural_gas_dummy, imports_electricity_dummy)

# transformari la nivel de date:
# initial, indicatorii ref la gaze naturale -- tj (terajouli)
# si indicatorii ref la electricitate -- gwh (gigawatti pe ora)
# am transformat indicatorii in twh
# fromule: 
# 1gwh = 1000twh
# 1twh = 3600tj

# y = consum de electricitate

# Instalarea pachetelor
PackageNames <- c("tidyverse", "stargazer", "magrittr", "tseries", "lmtest", "sandwich", 
                  "olsrr", "moments","whitestrap", "ggplot2", "DataCombine", "car", 
                  "caret", "splines","mgcv","glmnet","psych", "strucchange")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

install.packages("caret")

# Regresia simpla: consumption_electricity = beta0 + beta1*production_electricity + u
# x - productie electricitate
# y = 0.5799 * x + 1.8554
# beta1 = 0.5799
# testul t: p-value = 2.2e-16 < 0.001 => coef semnificativ
# testul F: p-value = 2.2e-16 < 0.1 => coef semnificativ
# r squared = 0.6015 (60%)
# adjusted r squared = 0.5991 (59%) => variabila production_electricity explica 59% din variatia modelului
model_energy <- lm(formula = consumption_electricity ~ production_electricity, data = energy)
summary(model_energy)
model_energy$coefficients['production_electricity']


# CERINTA 3
# Graficul observatiilor cu dreapta estimata
plot(x = energy$production_electricity, y = energy$consumption_electricity)
abline(a = model_energy$coefficients['(Intercept)'], 
       b = model_energy$coefficients['production_electricity'],
       col = 'red')

# pachetul GGPLOT2 care ne ajuta sa obtinem grafice mult mai aspectuase
ggplot(data = energy, mapping = aes(x = production_electricity, y = consumption_electricity)) +
  theme_bw() + # setarea temei graficului
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# Predictie dupa regresie 
# Utilizam datele energy si modelul 'model_energy'

# Valoarea estimata pentru variabila dependenta (consumption_electricityhat)
energy %<>% mutate(consumption_electricityhat = fitted(model_energy))
stargazer(energy, type = "text")
ggplot(data = energy, mapping = aes(x = production_electricity)) +
  geom_point(mapping = aes(y = consumption_electricity, color = 'Consumption_electricity - actual value')) +
  geom_point(mapping = aes(y = consumption_electricityhat, color = 'Consumption_electricityhat - predicted value')) + 
  xlab('production_electricity')

# Reziduuri
energy %<>% mutate(uhat = residuals(model_energy))
stargazer(energy, type = "text")
ggplot(energy, aes(x = production_electricity)) +
  geom_point(aes(y = consumption_electricity, col = 'Consumption_electricity - actual value')) +
  geom_point(aes(y = uhat, col = 'Residual uhat')) +
  xlab('production_electricity')

head(energy, 10)

# Graficul valorilor si reziduurilor reale si previzionate
ggplot(energy, aes(x = production_electricity)) +
  geom_point(aes(y = consumption_electricity, color = 'Consumption_electricity - actual value')) +
  geom_point(aes(y = consumption_electricityhat, color = 'Consumption_electricityhat - predicted value')) +
  geom_point(aes(y = uhat, color = 'Residual uhat')) +
  geom_smooth(aes(y = consumption_electricity, color = 'Fitted line'), 
              method = "lm", se = FALSE) +
  xlab('production_electricity')


# CERINTA 4
# Bonitatea modelului (R-squared)
# 'r.squared' din summary este R-squared
summary(model_energy)$r.squared

# histograma variabilei consumption_electrcity
# Histograma variabilei consum
ggplot(data = energy) +
  theme_bw() +
  geom_histogram(mapping = aes(x = consumption_electricity), col = 'grey') +
  xlab('consumption_electricity') + 
  ylab('Count') +
  ggtitle('Histograma variabilei consumption_electricity') + 
  theme(plot.title = element_text(hjust = 0.5))

# histograma variabilei log consum
ggplot(data = energy) +
  theme_bw() +
  geom_histogram(mapping = aes(x = log(consumption_electricity)), col = 'grey') +
  xlab('consumption_electricity') + 
  ylab('Count') +
  ggtitle('Histograma variabilei consumption_electricity') + 
  theme(plot.title = element_text(hjust = 0.5))



# Testarea ipotezelor ale modelului de regresie

energy %<>% mutate(uhat = resid(model_energy)) # extragem reziduurile din model
# 1. Normalitate

# Graficul Residuals vs Fitted
plot(model_energy)
# dreapta pare a fi (oarecum) dreapta si este apropiata de zero 
# (putem sa estimam ca reziduurile sunt normal distribuite)

# Graficul Q-Q plot
# diagrama formeaza o linie diagonala, deci reziduurile par sa aiba o distributie normala

# Histograma reziduurilor
ols_plot_resid_hist(model_energy)
# si din histograma reziduurile par sa aiba o distributie normala
# (valori centrate spre zero, curba in forma de clopot)
ggplot(data = energy) +
  theme_bw() +
  geom_histogram(mapping = aes(x = uhat), col = 'grey')+
  xlab('Reziduuri') + 
  ylab('Count') +
  ggtitle('Histograma reziduurilor') + 
  theme(plot.title = element_text(hjust = 0.5))

skewness(energy$uhat)
# asimetria = -0.2217 < 0 => distributie centrata la stanga

kurtosis(energy$uhat)
# boltirea = 2.89 < 3 => distributie leptocurtica (valoare apropiata de 3,
# ceea ce ne indica faptul ca distributia e normala)

# Boxplotul reziduurilor
boxplot(model_energy$residuals, main="Box Plot reziduuri")


# Testele de normalitate (Shapiro-Wilk si Jarque Bera)
# Testul Shapiro Wilk
# H0: distributie normala, H1: distributie nenormala
# W = 0.9917
# p-value = 0.4455 > 0.05 => H0 este acceptata => datele au o distributie normala
shapiro.test(energy$uhat)


# Testul Jarque-Bera (!!! - recomandat pt seturi de date cu mai mult de 50 de obs)
# H0: distributie normala, H1: distributie nenormala
# X-squared = 1.4515
# p-value = 0.484 > 0.05 => H0 este acceptata => datele au o distributie normala
jarque.bera.test(energy$uhat)
# ols_test_normality(model_energy)


# 2. Homoscedasticitate 
# (heteroscedasticitate = erorile nu sunt constante de-a lungul esantionului)

# Analiza grafica a heteroschedasticitatii
# Graficul reziduurilor fata de variabila independenta production_electricity
ggplot(data = energy, mapping = aes(x = production_electricity, y = uhat)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Productie electricitate')

# Graficul reziduurilor fata de valorile estimate de model
yhat <- fitted(model_energy)
energy %<>% mutate(yhat = yhat)
ggplot(data = energy, mapping = aes(x = yhat, y = uhat)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Valori estimate')
head(energy, 10)

# erorile nu sunt in forma de con (?), par sa fie homoscedastice

energy %<>% mutate(uhatsq = uhat^2,
                    yhatsq = yhat^2)

# Testele de heteroschedasticitate
model_energy <- lm(formula = consumption_electricity ~ production_electricity, data = energy)
summary(model_energy)

# Testul Breusch-Pagan 
# H0: erorile sunt homoscedastice (reziduurile sunt distribuite cu varianta egala)
# H1: erorile sunt heteroscedastice (reziduurile nu sunt distribuite cu varianta egala)
bptest(model_energy)
# BP = 0.4749
# p-values = 0.4907 > 0.05 => se accepta H0 => erorile sunt homoscedastice
# (erorile au varianta egala)

# Testul White 
# H0: erorile sunt homoscedastice (reziduurile sunt distribuite cu varianta egala)
# H1: erorile sunt heteroscedastice (reziduurile nu sunt distribuite cu varianta egala)
white_test(model_energy)
# p-values = 0.5384 > 0.05 => se accepta H0 => erorile sunt homoscedastice
# (erorile au varianta egala)


# 3. Non-autocorelare
# autocorelare = erorile sunt corelate cu ele insasi

# Metoda grafica de identificare a autocorelarii (ACF)
acf(model_energy$residuals)

# Testul Durbin-Watson (ordinul 1)
# H0: reziduurile nu sunt autocorelate
# H1: reziduurile sunt autocorelate 
dwtest(model_energy) 
# DW = 0.455
# d1 =  1.611 si d2 = 1.637 (n=150, k=1)
# DW < d1 => autocorelare pozitiva de ordin 1 a erorirlor
# p-value = 2.2e-16 < 0.1 => respingem ipoteza nula => reziduurile sunt autocorelate

# Testul Breusch-Godfrey (LM)
# H0: reziduurile nu sunt autocorelate
# H1: reziduurile sunt autocorelate de ordin r
bgtest(model_energy)
# LM = 99.731
# p-value = 2.2e-16 < 0.1 => respingem ipoteza nula => reziduurile sunt autocorelate
bgtest(model_energy, order = 2) 
bgtest(model_energy, order = 3)
# reziduurile sunt autocorelate si la lag superior

# Corectarea autocorelarii
energy2 <- read.csv("energy_romania7.csv")
energy2 %<>% select(consumption_electricity, production_electricity)
energy_data <- data.frame(energy2, resid_mod1=model_energy$residuals)
energy_data_1 <- slide(energy_data, Var="resid_mod1", NewVar = "lag1", slideBy = -1)
energy_data_2 <- na.omit(energy_data_1) 
head(energy_data_2, 10)

# Reimplementam modelul cu noua variabila lag1 adaugata in model
model_energy_2 <- lm(formula = consumption_electricity ~ production_electricity + lag1, data=energy_data_2)
summary(model_energy_2)

# Retestarea ipotezei pe modelul nou
# ACF 
acf(model_energy_2$residuals) # autocorelarea a disparut posibil??
# Durbin Watson 
dwtest(model_energy_2) 
# DW = 2.05
# p-value = 0.5843 > 0.1 => acceptam ipoteza nula => reziduurile nu sunt autocorelate
# Breusch-Godfrey 
bgtest(model_energy_2)
# p-value = 0.6 > 0.1 => acceptam ipoteza nula => reziduuri nonautocorelate 
bgtest(model_energy_2, order = 2)
# p-value = 0.1989 > 0.1 => reziduuri nonautocorelate
bgtest(model_energy_2, order = 3)
# p-value = 0.3515> 0.1 => reziduuri nonautocorelate
bgtest(model_energy_2, order = 4) # p-value = 0.37 > 0.1

# normalitate pentru noul model
# Testul Jarque-Bera (!!! - recomandat pt seturi de date cu mai mult de 50 de obs)
# H0: distributie normala, H1: distributie nenormala
# X-squared = 0.07
# p-value = 0.96 > 0.05 => H0 este acceptata => datele au o distributie normala
energy_data_2 %<>% mutate(uhat = resid(model_energy_2))
jarque.bera.test(energy_data_2$uhat)
# datele sunt distribuite normal

# homoscedasticitate pentru noul model
# Testul Breusch-Pagan 
# H0: erorile sunt homoscedastice (reziduurile sunt distribuite cu varianta egala)
# H1: erorile sunt heteroscedastice (reziduurile nu sunt distribuite cu varianta egala)
bptest(model_energy_2)
# BP = 0.4859
# p-values = 0.7843 > 0.05 => se accepta H0 => erorile sunt homoscedastice
# (erorile au varianta egala)

# Testul White 
# H0: erorile sunt homoscedastice (reziduurile sunt distribuite cu varianta egala)
# H1: erorile sunt heteroscedastice (reziduurile nu sunt distribuite cu varianta egala)
white_test(model_energy_2)
# p-values = 0.814 > 0.05 => se accepta H0 => erorile sunt homoscedastice
# (erorile au varianta egala)



# CERINTA 6
# Regresie multipla: 
# consumption_electricity = beta0 + beta1 * production_electricity + beta2 * consuption_natural_gas
# + beta3 * imports_electricity + u
# x1 - productie electricitate, x2 - consum de gaze naturale,
# x3 - importuri electricitate
# y = 0.49 * x1 + 0.01 * x2 + 0.33 * x3 + 1.76
# testul t:
# beta1 = 0.49, beta2 = 0.03, beta3 = 0.33
# beta1: p-value < 2.2e-16 < 0.001 => coef semnificativ
# beta2: p-value = 2.30e-12 < 0.001 => coef semnificativ
# beta3: p-value = 1.23e-06 < 0.001 => coef semnificativ
# testul F: p-value = 2.2e-16 < 0.001 => coef semnificativi
# r squared = 0.76
# adjusted r squared = 0.76 (76%) => variabilele explica 76% din variatia modelului
model_energy_multiple <- lm(formula = consumption_electricity ~ production_electricity
                            + consumption_natural_gas + imports_electricity, data=energy)
summary(model_energy_multiple)


# Testarea ipotezelor
energy %<>% mutate(uhat_multiple = resid(model_energy_multiple)) # extragem reziduurile din model
head(energy, 10)


# 1. Normalitate

# Graficul Residuals vs Fitted
plot(model_energy_multiple)
# dreapta pare a fi (oarecum) dreapta si este apropiata de zero 
# (putem sa estimam ca reziduurile sunt normal distribuite)

# Graficul Q-Q plot
# diagrama formeaza o oarecare linie diagonala, deci reziduurile par sa aiba o distributie normala

# Histograma reziduurilor
ols_plot_resid_hist(model_energy_multiple)
# si din histograma reziduurile par sa aiba o distributie normala
# (valori centrate spre zero, curba in forma de clopot)
ggplot(data = energy) +
  theme_bw() +
  geom_histogram(mapping = aes(x = uhat_multiple), col = 'grey')+
  xlab('Reziduuri') + 
  ylab('Count') +
  ggtitle('Histograma reziduurilor') + 
  theme(plot.title = element_text(hjust = 0.5))

skewness(energy$uhat_multiple)
# asimetria = -0.2211 < 0 => distributie centrata la stanga

kurtosis(energy$uhat_multiple)
# boltirea = 2.9 < 3 => distributie leptocurtica (valoare apropiata de 3,
# ceea ce ne indica faptul ca distributia e normala)

# Boxplotul reziduurilor
boxplot(model_energy_multiple$residuals, main="Box Plot reziduuri")


# Testele de normalitate (Shapiro-Wilk si Jarque Bera)
# Testul Shapiro Wilk
# H0: distributie normala, H1: distributie nenormala
# W = 0.9919
# p-value = 0.4721 > 0.05 => H0 este acceptata => datele au o distributie normala
shapiro.test(energy$uhat_multiple)


# Testul Jarque-Bera (!!! - recomandat pt seturi de date cu mai mult de 50 de obs)
# H0: distributie normala, H1: distributie nenormala
# X-squared = 1.4394
# p-value = 0.4869 > 0.05 => H0 este acceptata => datele au o distributie normala
jarque.bera.test(energy$uhat_multiple)
# ols_test_normality(model_energy)


# 2. Homoscedasticitate 
# (heteroscedasticitate = erorile nu sunt constante de-a lungul esantionului)

# Analiza grafica a heteroschedasticitatii
# Graficul reziduurilor fata de variabila independenta production_electricity
ggplot(data = energy, mapping = aes(x = production_electricity, y = uhat_multiple)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Productie electricitate')

# Graficul reziduurilor fata de variabila independenta consumption_natural_gas
ggplot(data = energy, mapping = aes(x = consumption_natural_gas, y = uhat_multiple)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Consum gaze naturale')

# Graficul reziduurilor fata de variabila independenta imports_electricity :(
ggplot(data = energy, mapping = aes(x = imports_electricity, y = uhat_multiple)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Importuri electricitate')

# Graficul reziduurilor fata de valorile estimate de model
yhat_multiple <- fitted(model_energy_multiple)
energy %<>% mutate(yhat_multiple = yhat_multiple)
ggplot(data = energy, mapping = aes(x = yhat_multiple, y = uhat_multiple)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Valori estimate')
head(energy, 10)

# erorile nu sunt in forma de con (?), par sa fie homoscedastice
# dar erorile fata de imports_electricity au o oarecare forma de con

energy %<>% mutate(uhatsq_multiple = uhat_multiple^2,
                   yhatsq_multiple = yhat_multiple^2)

# Testele de heteroschedasticitate
model_energy_multiple <- lm(formula = consumption_electricity ~ production_electricity + consumption_natural_gas 
                            + imports_electricity, data=energy)
summary(model_energy_multiple)

# Testul Breusch-Pagan 
# H0: erorile sunt homoscedastice (reziduurile sunt distribuite cu varianta egala)
# H1: erorile sunt heteroscedastice (reziduurile nu sunt distribuite cu varianta egala)
bptest(model_energy_multiple)
# BP = 5.2095
# p-values = 0.1571 > 0.05 => se accepta H0 => erorile sunt homoscedastice
# (erorile au varianta egala)

# Testul White 
# H0: erorile sunt homoscedastice (reziduurile sunt distribuite cu varianta egala)
# H1: erorile sunt heteroscedastice (reziduurile nu sunt distribuite cu varianta egala)
white_test(model_energy_multiple)
# p-values = 0.7253 > 0.05 => se accepta H0 => erorile sunt homoscedastice
# (erorile au varianta egala)


# 3. Multicoliniaritate
# Calculul matricii de corelatie
energy %>% 
  select(production_electricity, consumption_natural_gas, imports_electricity) %>% 
  na.omit %>% 
  cor
# nu exista valori absolute > 0.85 => nu exista multicoliniaritate

vif(model_energy_multiple)
# vif pt production_electricity = 2.0397
# vif pt consumption_natural_gas = 1.6411
# vif pt imports_electricity = 1.35
# toate < 10 => nu exista multicoliniaritate


# 4. Non-autocorelare
# autocorelare = erorile sunt corelate cu ele insasi

# Metoda grafica de identificare a autocorelarii (ACF)
acf(model_energy_multiple$residuals)

# Testul Durbin-Watson (ordinul 1)
# H0: reziduurile nu sunt autocorelate
# H1: reziduurile sunt autocorelate 
dwtest(model_energy_multiple) 
# DW = 0.5733
# d1 =  1.584 si d2 = 1.665 (n=150, k=3)
# DW < d1 => autocorelare pozitiva de ordin 1 a erorilor
# p-value = 2.2e-16 < 0.1 => respingem ipoteza nula => reziduurile sunt autocorelate

# Testul Breusch-Godfrey (LM)
# H0: reziduurile nu sunt autocorelate
# H1: reziduurile sunt autocorelate de ordin r
bgtest(model_energy_multiple)
# LM = 90.338
# p-value = 2.2e-16 < 0.1 => respingem ipoteza nula => reziduurile sunt autocorelate
bgtest(model_energy_multiple, order = 2) 
bgtest(model_energy_multiple, order = 3)
# reziduurile sunt autocorelate si la lag superior

# Corectarea autocorelarii
energy2_multiple <- read.csv("energy_romania7.csv")
energy2_multiple %>% select(consumption_electricity, production_electricity, consumption_natural_gas, imports_electricity)
energy_data_multiple <- data.frame(energy2_multiple, resid_mod1=model_energy_multiple$residuals)
energy_data_1_multiple <- slide(energy_data_multiple, Var="resid_mod1", NewVar = "lag1", slideBy = -1)
energy_data_2_multiple <- na.omit(energy_data_1_multiple) 
head(energy_data_2_multiple, 10)

# Reimplementam modelul cu noua variabila lag1 adaugata in model
model_energy_multiple_2 <- lm(formula = consumption_electricity ~ production_electricity + 
                                consumption_natural_gas + imports_electricity + lag1, data=energy_data_2_multiple)
summary(model_energy_multiple_2)

# Retestarea ipotezei pe modelul nou
# ACF 
acf(model_energy_multiple_2$residuals) # autocorelarea a disparut posibil??
# Durbin Watson 
dwtest(model_energy_multiple_2) 
# DW = 2.13
# p-value = 0.732 > 0.1 => acceptam ipoteza nula => reziduurile nu sunt autocorelate
# Breusch-Godfrey 
bgtest(model_energy_multiple_2)
# p-value = 0.22 > 0.1 => acceptam ipoteza nula => reziduuri nonautocorelate 
bgtest(model_energy_multiple_2, order = 2)
# p-value = 0.08 > 0.05 => reziduuri nonautocorelate (95%)
bgtest(model_energy_multiple_2, order = 3)
# p-value = 0.07 > 0.05 => reziduuri nonautocorelate (95%)



# CERINTA 7

# Modificarea modelului de regresie multipla prin adoptarea unor alte
# forme functionale
# log-log
# log(consumption_electricity) = beta0 + beta1 * log(production_electricity) + beta2 * log(consuption_natural_gas)
# + beta3 * log(imports_electricity) + u
# x1 - productie electricitate, x2 - consum de gaze naturale,
# x3 - importuri electricitate, x4 - importuri gaze naturale (var dummy)
# y = 0.48 * x1 + 0.08 * x2 + 0.1 * x3 + 0.6
# testul t:
# beta1 = 0.48, beta2 = 0.08, beta3 = 0.1
# beta1: p-value < 2.2e-16 < 0.001 => coef semnificativ
# beta2: p-value = 3.56e-12 < 0.001 => coef semnificativ
# beta3: p-value = 2.29e-07 < 0.001 => coef semnificativ
# testul F: p-value < 2.2e-16 < 0.1 => coef semnificativi
# r squared = 0.73
# adjusted r squared = 0.73 (73%) => variabilele explica 76% din variatia modelului
# consumul de electricitate creste cu 48% atunci cand productia de electricitate creste cu 1%
# consumul de electricitate creste cu 8.64% atunci cand consumul de gaz natural creste cu 1%
# consumul de electricitate creste cu 1.5% atunci cand importurile de electricitate cresc cu 1%
model_energy_loglog <- lm(formula = log(consumption_electricity) ~ log(production_electricity) + 
                            log(consumption_natural_gas) + log(imports_electricity), data=energy)
summary(model_energy_loglog)

# testele de normalitate (Shapiro-Wilk si Jarque Bera)
# Testul Shapiro Wilk
# H0: distributie normala, H1: distributie nenormala
# W = 0.98
# p-value = 0.27 > 0.1 => H0 este acceptata => datele au o distributie normala
energy_log <- read.csv("energy_romania7.csv")
energy_log %<>% select(consumption_electricity, consumption_natural_gas,
                         production_electricity, imports_electricity)
energy_log %<>% mutate(uhat_loglog = resid(model_energy_loglog), consumption_electricity_log = log(consumption_electricity),
                          consumption_natural_gas_log = log(consumption_natural_gas), 
                          production_electricity_log = log(production_electricity),
                          imports_electricity_log = log(imports_electricity))
shapiro.test(energy_log$uhat_loglog)

# Testul Jarque-Bera
# H0: distributie normala, H1: distributie nenormala
# X-squared = 2.1
# p-value = 0.33 > 0.1 => H0 este acceptata => datele au o distributie normala
jarque.bera.test(energy_log$uhat_loglog)
# ols_test_normality(model_energy)


# teste de heteroschedasticitate
# Testul Breusch-Pagan 
# H0: erorile sunt homoscedastice (reziduurile sunt distribuite cu varianta egala)
# H1: erorile sunt heteroscedastice (reziduurile nu sunt distribuite cu varianta egala)
bptest(model_energy_loglog)
# BP = 8.2269
# p-values = 0.08 > 0.05 => se accepta H0 => erorile sunt homoscedastice
# (erorile au varianta egala)

# Testul White 
# H0: erorile sunt homoscedastice (reziduurile sunt distribuite cu varianta egala)
# H1: erorile sunt heteroscedastice (reziduurile nu sunt distribuite cu varianta egala)
white_test(model_energy_loglog)
# p-values = 0.9 > 0.05 => se accepta H0 => erorile sunt homoscedastice
# (erorile au varianta egala)


# teste de multicoliniaritate
# Calculul matricii de corelatie
energy_log %>% 
  select(production_electricity_log, consumption_natural_gas_log, 
         imports_electricity_log) %>% 
  na.omit %>% 
  cor
# nu exista valori absolute > 0.85 => nu exista multicoliniaritate

vif(model_energy_loglog)
# vif pt production_electricity = 1.64
# vif pt consumption_natural_gas = 1.5
# vif pt imports_electricity = 1.1
# toate < 10 => nu exista multicoliniaritate


# teste de autocorelare
acf(model_energy_loglog$residuals)

# Durbin-Watson (ordinul 1)
dwtest(model_energy_loglog) 
# DW = 0.6
# d1 =   1.571 si d2 = 1.679 (n=150, k=4)
# DW < d1 => autocorelare pozitiva de ordin 1 a erorilor
# p-value < 2.2e-16 < 0.1 => reziduuri autocorelate

# Breusch-Godfrey (LM)
bgtest(model_energy_loglog)
# LM = 85.913
# p-value < 2.2e-16 < 0.1 => respingem ipoteza nula => reziduurile sunt autocorelate
bgtest(model_energy_loglog, order = 2) 
bgtest(model_energy_loglog, order = 3)
# reziduurile sunt autocorelate si la lag superior

# lin-log
# consumption_electricity = beta0 + beta1 * log(production_electricity) + beta2 * log(consuption_natural_gas)
# + beta3 * log(imports_electricity) + u
# x1 - productie electricitate, x2 - consum de gaze naturale,
# x3 - importuri electricitate, x4 - importuri gaze naturale (var dummy)
# y = 2.23 * x1 + 0.41 * x2 + 0.07 * x3 + 0.31
# testul t:
# beta1 = 2.23, beta2 = 0.41, beta3 = 0.01
# beta1: p-value < 2.2e-16 < 0.001 => coef semnificativ
# beta2: p-value = 1.34e-12 < 0.001 => coef semnificativ
# beta3: p-value = 4.46e-07 < 0.001 => coef semnificativ
# testul F: p-value < 2.2e-16 < 0.1 => coef semnificativi
# r squared = 0.74
# adjusted r squared = 0.73 (73%) => variabilele explica 76% din variatia modelului
# consumul de electricitate creste cu 0.022twh (22.3gwh) atunci cand productia de electricitate creste cu 1%
# consumul de electricitate creste cu 0.004twh (4.11gwh) atunci cand consumul de gaz natural creste cu 1%
# consumul de electricitate creste cu 0.003twh (3.19gwh) atunci cand importurile de electricitate cresc cu 1%
model_energy_linlog <- lm(formula = consumption_electricity ~ log(production_electricity) + 
                            log(consumption_natural_gas) + log(imports_electricity), data=energy)
summary(model_energy_linlog)



# Adaugarea variabilei imports_natural_gas_dummy la modelul de regresie multipla
# am transformat in excel variabila imports_natural_gas (care, cand am testat-o initial intr-un model
# de regresie simpla, nu reprezenta un coef semnificativ) intr-una dummy astfel: 
# - daca imports < mean(imports) -> 0
# - daca imports >= mean(imports) -> 1
# (mean(imports) = 6.769)
# consumption_electricity = beta0 + beta1 * production_electricity + beta2 * consuption_natural_gas
# + beta3 * imports_electricity + beta4 * imports_natural_gas_dummy + u
# x1 - productie electricitate, x2 - consum de gaze naturale,
# x3 - importuri electricitate, x4 - importuri gaze naturale (var dummy)
# y = 0.51 * x1 + 0.03 * x2 + 0.31 * x3 + 0.08 * x4 + 1.72
# testul t:
# beta1 = 0.51, beta2 = 0.03, beta3 = 0.31, beta4 = 0.08
# beta1: p-value < 2.2e-16 < 0.001 => coef semnificativ
# beta2: p-value = 7.94e-07 < 0.001 => coef semnificativ
# beta3: p-value = 4.69e-06 < 0.001 => coef semnificativ
# beta4: p-value = 0.0167 < 0.1 => coef semnificativ
# testul F: p-value < 2.2e-16 < 0.1 => coef semnificativi
# r squared = 0.77
# adjusted r squared = 0.7672 (76%) => variabilele explica 76% din variatia modelului
rm(list = ls())
energy_dummy <- read.csv("energy_romania7.csv")
energy_dummy %<>% select(consumption_electricity, consumption_natural_gas,
                  production_electricity, imports_electricity,
                  imports_natural_gas_dummy)
model_energy_dummy <- lm(formula = consumption_electricity ~ production_electricity + consumption_natural_gas + 
                           + imports_electricity + imports_natural_gas_dummy, data=energy_dummy)
summary(model_energy_dummy)


# testele de normalitate (Shapiro-Wilk si Jarque Bera)
# Testul Shapiro Wilk
# H0: distributie normala, H1: distributie nenormala
# W = 0.99
# p-value = 0.57 > 0.1 => H0 este acceptata => datele au o distributie normala
energy_dummy %<>% mutate(uhat = resid(model_energy_dummy))
shapiro.test(energy_dummy$uhat)

# Testul Jarque-Bera
# H0: distributie normala, H1: distributie nenormala
# X-squared = 1.38
# p-value = 0.5 > 0.1 => H0 este acceptata => datele au o distributie normala
jarque.bera.test(energy_dummy$uhat)
# ols_test_normality(model_energy)


# teste de heteroschedasticitate
# Testul Breusch-Pagan 
# H0: erorile sunt homoscedastice (reziduurile sunt distribuite cu varianta egala)
# H1: erorile sunt heteroscedastice (reziduurile nu sunt distribuite cu varianta egala)
bptest(model_energy_dummy)
# BP = 8.2269
# p-values = 0.08 > 0.05 => se accepta H0 => erorile sunt homoscedastice
# (erorile au varianta egala)

# Testul White 
# H0: erorile sunt homoscedastice (reziduurile sunt distribuite cu varianta egala)
# H1: erorile sunt heteroscedastice (reziduurile nu sunt distribuite cu varianta egala)
white_test(model_energy_dummy)
# p-values = 0.9 > 0.05 => se accepta H0 => erorile sunt homoscedastice
# (erorile au varianta egala)


# teste de multicoliniaritate
# Calculul matricii de corelatie
energy_dummy %>% 
  select(production_electricity, consumption_natural_gas, 
         imports_electricity, imports_natural_gas_dummy) %>% 
  na.omit %>% 
  cor
# nu exista valori absolute > 0.85 => nu exista multicoliniaritate

vif(model_energy_dummy)
# vif pt production_electricity = 2.13
# vif pt consumption_natural_gas = 2.33
# vif pt imports_electricity = 1.37
# vif pt imports_natural_gas_dummy = 1.54
# toate < 10 => nu exista multicoliniaritate


# teste de autocorelare
acf(model_energy_dummy$residuals)

# Durbin-Watson (ordinul 1)
dwtest(model_energy_dummy) 
# DW = 0.6
# d1 =   1.571 si d2 = 1.679 (n=150, k=4)
# DW < d1 => autocorelare pozitiva de ordin 1 a erorilor
# p-value < 2.2e-16 < 0.1 => reziduuri autocorelate

# Breusch-Godfrey (LM)
bgtest(model_energy_dummy)
# LM = 86.531
# p-value < 2.2e-16 < 0.1 => respingem ipoteza nula => reziduurile sunt autocorelate
bgtest(model_energy_dummy, order = 2) 
bgtest(model_energy_dummy, order = 3)
# reziduurile sunt autocorelate si la lag superior

# Corectarea autocorelarii
energy2_dummy <- read.csv("energy_romania7.csv")
energy2_dummy %>% select(consumption_electricity, production_electricity, consumption_natural_gas, imports_electricity,
                         imports_natural_gas_dummy)
energy_data_dummy <- data.frame(energy2_dummy, resid_mod1=model_energy_dummy$residuals)
energy_data_1_dummy <- slide(energy_data_dummy, Var="resid_mod1", NewVar = "lag1", slideBy = -1)
energy_data_2_dummy <- na.omit(energy_data_1_dummy) 
head(energy_data_1_dummy, 10)

# Reimplementam modelul cu noua variabila lag1 adaugata in model
model_energy_dummy_2 <- lm(formula = consumption_electricity ~ production_electricity + 
                          consumption_natural_gas + imports_electricity + imports_natural_gas_dummy 
                           + lag1, data=energy_data_2_dummy)
summary(model_energy_dummy_2)


# Retestarea ipotezei pe modelul nou
# ACF 
acf(model_energy_dummy_2$residuals) # autocorelarea a disparut posibil??
# Durbin Watson 
dwtest(model_energy_dummy_2) 
# DW = 2.09
# p-value = 0.62 > 0.1 => acceptam ipoteza nula => reziduurile nu sunt autocorelate
# Breusch-Godfrey 
bgtest(model_energy_dummy_2)
# p-value = 0.37 > 0.1 => acceptam ipoteza nula => reziduuri nonautocorelate 
bgtest(model_energy_dummy_2, order = 2)
# p-value = 0.09 > 0.05 => reziduurile nu sunt autocorelate (95%)
bgtest(model_energy_dummy_2, order = 3)
# p-value = 0.066 > 0.05 => reziduurile nu sunt autocorelate (95%)
bgtest(model_energy_dummy_2, order = 4) # autocorelate la ordinul 4 (??)



# Termeni de interactiune
# consumption_electricity = beta0 + beta1 * production_electricity + beta2 * consuption_natural_gas
# + beta3 * imports_electricity + beta4 * production_electricity*imports_electricity + u
# x1 - productie electricitate, x2 - consum de gaze naturale,
# x3 - importuri electricitate
# x4 - productie electricitate * importuri electricitate
# y = 0.56 * x1 + 0.03 * x2 + 1.85 * x3 + (-0.33) * x3 * x1 + 1.4
# testul t:
# beta1 = 0.56, beta2 = 0.03, beta3 = 1.85, beta4 = -0.33
# beta1: p-value < 2.2e-16 < 0.001 => coef semnificativ
# beta2: p-value = 2.87e-13 < 0.001 => coef semnificativ
# beta3: p-value = 0.002 < 0.01 => coef semnificativ
# beta4: p-value = 0.01 < 0.05 => coef semnificativ
# testul F: p-value = 2.2e-16 < 0.001 => coef semnificativi
# r squared = 0.77
# adjusted r squared = 0.76 (76%) => variabilele explica 76% din variatia modelului
model_energy_int <- lm(formula = consumption_electricity ~ production_electricity
                            + consumption_natural_gas + imports_electricity
                           + production_electricity*imports_electricity, data=energy)
summary(model_energy_int)
# coef pt productia de energie electrica e pozitiv si semnificativ => consumul de energie electrica
# creste cand productia de energie electrica creste
# coef pt importurile de energie electrica e pozitiv si semnificativ => consumul de energie electrica
# scade cand importurile de energie electrica cresc
# coeficientul lui x4 este negativ si semnificativ: 
# 1. productia de energie electrica si importurile de energie electrica
# nu se "ajuta" unul pe altul
# 2. consumul de energie electrica scade atunci cand productia si importurile de energie
# electrica creste (?)

model_energy_int <- lm(formula = consumption_electricity ~ production_electricity
                       + consumption_natural_gas + imports_electricity
                       + production_natural_gas*consumption_natural_gas, data=energy)
summary(model_energy_int)
# coef pt consumul de gaze naturale e negativ si semnificativ => consumul de energie electrica
# scade cand consumul de gaze naturale creste
# coef pt productia de gaze naturale e negativ si semnificativ => consumul de energie electrica
# scade cand productia de gaze naturale creste
# in acest caz coeficientul lui production_natural_gas*consumption_natural_gas
# este pozitiv si semnificativ => 
# 1. productia de gaze naturale accentueaza impactul consumului de gaze naturale
# 2. consumul de energie electrica creste atunci cand consumul si productia de gaze
# naturale creste



# Realizarea de prognoze
# Prognoza pe modelul ce include variabila dummy 
library(tidyverse)
library(caret)
install.packages("mltools")
install.packages("MLmetrics")
library(MLmetrics)
library(mltools)
# realizarea seturilor de antrenare (80%) si de testare (20%)
set.seed(123)
training.samples <- energy_data_2_dummy$consumption_electricity %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- energy_data_2_dummy[training.samples, ]
test.data <- energy_data_2_dummy[-training.samples, ]

model_energy_dummy_3 <- lm(formula = consumption_electricity ~ production_electricity + 
                             consumption_natural_gas + imports_electricity + imports_natural_gas_dummy 
                           + lag1, data=train.data)
summary(model_energy_dummy_3)

# Predictia modelului pe setul de testare
y_pred <- predict(model_energy_dummy_3, newdata = test.data)
y_pred

# Calculul indicatorilor 
# RMSE - Root Mean Squared Error
RMSE(y_pred, test.data$consumption_electricity)
# RMSE = 0.15 < 1 => predictie buna

# MAE - Mean Absolute Error 
MAE(y_pred, test.data$consumption_electricity)
# MAE = 0.13 < 1 => predictie buna

# MSE - Mean Squared Error
mse(y_pred, test.data$consumption_electricity)
# MSE = 0.023 < 1 => predictie buna

# MAPE - Mean Absolute Percentage Error
MAPE(y_pred, test.data$consumption_electricity)
# MAPE = 0.028 < 1 => predictie buna

# Predictia modelului pentru date din afara setului de testare 
# (pe modelul necorectat)
out_of_sample <- data.frame(production_electricity = c(3, 7, 8),
                            consumption_natural_gas = c(20, 25, 30),
                            imports_electricity = c(0.8, 0.9, 1),
                            imports_natural_gas_dummy = c(1, 1, 1))

y_pred_outsample <- predict(model_energy_dummy, newdata = out_of_sample)
y_pred_outsample
# valori: 4.213178, 6.454224, 7.152492 




# CERINTA 9
# 1. Modele de penalizare
# regresia Ridge
energy_dummy <- read.csv("energy_romania7.csv")
View(energy_dummy)
energy_dummy %<>% select(consumption_electricity, consumption_natural_gas,
                         production_electricity, imports_electricity,
                         imports_natural_gas_dummy, production_natural_gas)
model0 <- lm(formula = consumption_electricity ~ production_electricity + consumption_natural_gas + 
                           + imports_electricity + imports_natural_gas_dummy + production_natural_gas, data=energy_dummy)
summary(model0)
# coeficientii variabilelor pentru modelul realiz prin regresie multipla:
# production_electricity = 0.49
# consumption_natural_gas = 0.02
# imports_electricity = 0.34
# imports_natural_gas_dummy = 0.09
# production_natural_gas = 0.02 (si nesemnificativ)
# termentul liber = 1.55
# rsquared = 0.7741 (77.41%)

# variabila raspuns
y <- energy_dummy$consumption_electricity
# predictorii
x <- data.matrix(energy_dummy[, c('production_electricity', 'consumption_natural_gas', 
                           'imports_electricity', 'imports_natural_gas_dummy',
                           'production_natural_gas')])

# estimarea modelului ridge (alpha = 0)
model_ridge <- glmnet(x, y, alpha = 0)
summary(model_ridge)

# identificarea valoarii lui lambda pt care MSE e minim
# (se foloseste validarea incrucisata - cross validation)
cv_model_ridge <- cv.glmnet(x, y, alpha = 0)
best_lambda <- cv_model_ridge$lambda.min
best_lambda # 0.02

# testarea valorii lamda 
plot(cv_model_ridge) 

# reimplementarea modelului cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model) 
# coeficientii variabilelor:
# production_electricity = 0.42
# consumption_natural_gas = 0.02
# imports_electricity = 0.28
# imports_natural_gas_dummy = 0.09
# production_natural_gas = 0.04
# intercept = 1.73

# Diagrama Trace pentru a vizualiza modul in care estimarile coeficientulilor s-au
# modificat ca urmare a cresterii valorii lui lambda
plot(model_ridge, xvar = "lambda")
legend("topright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

# testare 
y_predicted <- predict(model_ridge, s = best_lambda, newx = x)

# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq
# r squared = 0.7699 (76.99%)


# regresia LASSO
# estimarea modelului lasso (alpha = 1)
model_lasso <- glmnet(x, y, alpha = 1)

# identificarea valoarii optime ale lui lambda
cv_model_lasso <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_model_lasso$lambda.min
best_lambda # 0.0009

# testarea valorii lamda
plot(cv_model_lasso) 

# reimplementarea modelului cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) 
# coeficientii variabilelor 
# production_electricity = 0.49
# consumption_natural_gas = 0.02
# imports_electricity = 0.33
# imports_natural_gas_dummy = 0.09
# production_natural_gas = 0.02
# intercept = 1.58

# Diagrama Trace pentru a vizualiza modul in care estimarile coeficientulilor s-au
# modificat ca urmare a cresterii valorii lui lambda
plot(model_lasso, xvar = "lambda",label=T)
legend("topright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

# Prognoze 
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq 
# r squared = 0.774 (77.4%)

# regresia Elastic Net
# estimarea modelului elcastic net (alpha = 0.5)
model_elasticnet <- cv.glmnet(x, y, alpha = 0.5)

# Valoarea optima a lui lambda
cv_model_elasticnet <- cv.glmnet(x, y, alpha = 0.5)
best_lambda <- cv_model_elasticnet$lambda.min
best_lambda # 0.002

# testarea valorii lamda
plot(cv_model_elasticnet) 

# Reimplementam modelul cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 0.5, lambda = best_lambda)
coef(best_model)
# coeficientii variabilelor 
# production_electricity = 0.48
# consumption_natural_gas = 0.02
# imports_electricity = 0.33
# imports_natural_gas_dummy = 0.09
# production_natural_gas = 0.02
# intercept = 1.59

# Diagrama Trace pentru a vizualiza modul in care estimarile coeficientulilor s-au
# modificat ca urmare a cresterii valorii lui lambda
plot(model_elasticnet, xvar = "lambda")
legend("topright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

# Prognoze 
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq 
# r squared = 0.774 (77.4%)


# 2. Model obtinut prin intermediul metodei de selectie Boruta   
install.packages("Boruta")
library(Boruta)

set.seed(111)
boruta.bank_train <- Boruta(consumption_electricity~., data = energy_dummy, doTrace = 2)
print(boruta.bank_train)

# Vom selecta atributele importante 
getSelectedAttributes(boruta.bank_train, withTentative = T)
# variabilele selectate sunt:
# consumption_natural_gas, production_electricity, imports_electricity,
# imports_natural_gas_dummy, production_natural_gas
# toate atributele sunt considerate importante

model_boruta <- lm(consumption_electricity ~ production_electricity + consumption_natural_gas + 
                     + imports_electricity + imports_natural_gas_dummy + production_natural_gas, energy_dummy)
summary(model_boruta) 
# rsquared = 0.7671 (76.71%)


# modelul optim estei modelul obtinut prin regresia elastic net sau prin LASSO, care, in comparatie
# cu modelul nostru de regresie multipla, contine si productia de gaze naturale 
# motitv: indicele de bonitate (rsquared) este maxim (77.41%)
