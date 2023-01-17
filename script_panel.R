setwd('D:/facultate/cursuri/anul_3/sem1/econometrie/proiect/cod/electricity')

rm(list = ls()) 

PackageNames <- c("tidyverse","gplots","plm","readxl","foreign","lmtest",
                  "magrittr")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

energy_panel <- read.csv("energy_panel2.csv")
View(energy_panel)
energy_panel %<>% select(country, month, consumption_electricity, consumption_natural_gas,
                   production_electricity, imports_electricity)

# transformari la nivel de date:
# initial, indicatorii ref la gaze naturale -- tj (terajouli)
# si indicatorii ref la electricitate -- gwh (gigawatti pe ora)
# am transformat indicatorii in twh
# fromule: 
# 1000gwh = 1twh
# 1twh = 3600tj

summary(energy_panel)

# Explorarea datelot panel
pd.df <- pdata.frame(energy_panel, index = c("country","month"), drop.index = TRUE)

# corealatia dintre consumul de energie electrica, spatiu si timp
# coplot(consumption_electricity ~ month|country, type="l", data=energy_panel) 

# Heterogenitatea in sectiunea spatiala
# Graficul traseaza un interval de incredere de 95% in jurul mediilor
# avem tari pentru care ratele sunt mai ridicate (ex: Franta, Italia)
# si tari pentru care ratele sunt mai scazute (ex: Croatia, Bulgaria)
# datele par sa varieze, deci datele sunt heterogene in sectiunea spatiala
plotmeans(consumption_electricity ~ country, main = 'Heterogeneitate in randul tarilor', data = energy_panel)


# Heterogenitatea in sectiunea temportala
# luni pentru care ratele sunt mai ridicate (ex: 2019-01)
# si luni pentru care ratele sunt mai scazute (ex: 2020-05)
# avem date heterogene
plotmeans(consumption_electricity ~ month, main = 'Heterogeneitate in timp', data = energy_panel)

# Modelul OLS
model_ols <- lm(consumption_electricity ~ production_electricity + consumption_natural_gas 
          + imports_electricity, energy_panel)
summary(model_ols)
# nu ia in considerare heterogenitatea in spatiu si timp
# toti coeficientii sunt semnificativi
# r-squared = 0.99 => variabilele explica 99% din variatia consumului de energie electrica

# Modelul Fixed-Effects (FE)
model_fe <- plm(consumption_electricity ~ production_electricity + consumption_natural_gas 
                + imports_electricity, energy_panel, index = c('country','month'),
            model = 'within')
summary(model_fe)
# n = 10 tari
# T = 60 luni
# N = 600 obervatii
# modelul: consumption_electricity = production_electricity * beta1 + consumption_natural_gas * beta2
#          + imports_electricity * beta3 + u
# y = consumption_electricity
# x1 = production_electricity
# beta1 = 0.85
# p-value < 2e-16 < 0.05 => coeficient semnificativ
# x2 = consumption_natural_gas
# beta2 = 0.0022
# p-value = 0.35 > 0.05 => coeficient nesemnificativ
# x3 = imports_electricity
# beta3 = 1.37
# p-value < 2e-16 < 0.05 => coeficient nesemnificativ
# consumul de gaze naturale este singura variabila care nu are impact asupra 
# consumului de energie electrica

model_fe2 <- plm(consumption_electricity ~ production_electricity, 
                energy_panel, index = c('country','month'),
                model = 'within')
summary(model_fe2)
# modelul: consumption_electricity = production_electricity * beta1 + u
# y = consumption_electricity
# x1 = production_electricity
# beta1 = 0.89
# Adjusted r-squared = 0.85 => modelul explica 85% din variatia consumului de energie electrica
# (r-squared e influentat de efecte)

# Modelul Random-Effects (RE)
model_re <- plm(consumption_electricity ~ production_electricity, 
                 energy_panel, index = c('country','month'),
                 model = 'between')
summary(model_re)
# modelul: consumption_electricity = production_electricity * beta1 + imports_electricity * beta2 + u
# y = consumption_electricity
# x1 = production_electricity
# beta1 = 0.86
# p-value = 2.394e-07 < 0.05 => coeficient semnificativ
# Adjusted r-squared = 0.96 => modelul explica 96% din variatia consumului de energie electrica

# Testul Hausman 
# il folosim pentru alegerea tipului de model
# H0: model cu efecte random 
# H1: model cu efecte fixe
phtest(model_fe2, model_re)
# p-value = 0.63 > 0.05 => acceptam ipoteza nula
# vom folosi modelul cu efecte random (RE) 


# modelul RE
model_re <- plm(consumption_electricity ~ production_electricity, 
                energy_panel, index = c('country','month'),
                model = 'between')
summary(model_re)

# Testarea efectelor aleatoare in timp
# H0: nu sunt necesare efectele aleatoare in timp
# H1: sunt necesare efectele aleatoare in timp
# testul F
pFtest(model_re, model_ols) 
# p-value = 1 => se accepta H0 => nu se recomanda folosirea efectelor random ??
# testul LM
plmtest(model_re, c('time'), type = 'bp')
# p-value = 5.742e-08 < 0.05 => se respinge H0 => se recomanda folosirea efectelor random
# vom utiliza efecte aleatoare


# Testarea efectelor aleatorii cu Breusch-Pagan Lagrange Multiplier
# H0: variatiile in timp sunt 0
# H1: variatiile in timp sunt diferite de 0
pool <- plm(consumption_electricity ~ production_electricity, 
            data=energy_panel, index=c("country", "month"), model="pooling")
summary(pool)
plmtest(pool, type=c("bp"))
# p-value < 2.2e-16 < 0.05 => se respinge H0 => variatiile in timp sunt diferite de 0
# efectele aleatoare sunt adecvate ai exista diferente semnificative intre tari


# Testarea dependentei transversale folosind testul Breusch-Pagan LM si 
# testul Parasan CD
# Ipoteze teste
# H0: reziduurile intre entitati nu sunt corelate
# H1: reziduurile intre entitati sunt corelate


# Testarea ipotezelor modelului
# 1. Autocorelare
# Testul Breusch-Godfrey/Wooldridge
# H0: datele nu sunt autocorelate
# H1: datele sunt autocorelate
pbgtest(model_re) # ??
# Error in resi[1:(n - x)] : only 0's may be mixed with negative subscripts

# Testul Durbin-Watson 
pdwtest(model_re)
# p-value = 0.81 > 0.05 => se accepta H0 => datele nu sunt autocorelate


# 2. Heteroscedasticitate
# Testul Breusch-Pagan
# H0: datele sunt homoschedastice
# H1: datele sunt heteroschedastice
bptest(consumption_electricity ~ production_electricity + factor(month), data = energy_panel, studentize=F) 
# p-value < 2.2e-16 < 0.05 => H0 este respinsa => datele sunt heteroschedastice

