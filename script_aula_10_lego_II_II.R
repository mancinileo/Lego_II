##======================================
##
##         Aula 10 Lego II
##
##======================================

## abrir todos pacotes. antes, rodar:
## install.packages("pacman")

pacman::p_load(tidyverse, rio, foreign, haven)

## importar base ========================================

base_idh <- import("IDH_2007.dta")

## saber nomes das variaveis ============================

names(base_idh)

## cortar base so com as variaveis necessarias =================

base_idh <- base_idh %>% 
  select(gini, fert2005, gdpcap05, 
         tempera)

## sumarizar dados =============================================

summary(base_idh)

## acertar outliers ============================================

base_idh <- base_idh %>% 
  filter(gini != 999, fert2005 != 999, gdpcap05 != 999999)  

## "attach" so para nao escrever o nome da base o tempo todo ===

attach(base_idh)

mod1 <- lm(fert2005 ~ gdpcap05)

summary(mod1)

mod2 <- lm(fert2005 ~ gdpcap05 + gini)

summary(mod2)

mod3 <- lm(fert2005 ~ gdpcap05 + gini + tempera)

summary(mod3)

mod4 <- lm(fert2005 ~ gdpcap05*tempera + gini)

summary(mod4)

mod5 <- lm(fert2005 ~ gdpcap05*tempera)

summary(mod5)

## modelo com transformacoes logaritmicas =======================

mod6 <- lm(log(fert2005) ~ log(gdpcap05)*tempera)

summary(mod6)

## quanto menor o indice, melhor ================================

AIC(mod1,mod2,mod3,mod4,mod5,mod6)
BIC(mod1,mod2,mod3,mod4,mod5,mod6)


## outra forma de comparar: anova ==============================

anova(mod4, mod5)
anova(mod5, mod6)

## outra forma de comparar: validacao externa ==================


library(caret)

## cortar base em duas partes: treino (70%) e teste (30%) ======

train <- createDataPartition(base_idh$fert2005, 
                             p=0.7, list = F)

training <- base_idh[train, ]
test <- base_idh[-train, ]

training <- data.frame(training)
test <- data.frame(test)

## rodar modelos na base de treino ==============================

mod4 <- lm(fert2005 ~ gdpcap05*tempera + gini, 
           data = training)
mod5 <- lm(fert2005 ~ gdpcap05*tempera, 
           data = training)
mod6 <- lm(log(fert2005) ~ log(gdpcap05)*tempera, 
           data = training)

output4 <- predict(mod4, test)
output5 <- predict(mod5, test)
output6 <- predict(mod6, test)

library(Metrics)

## usar o rmse como medida de erro em relacao aos valores reais. 
## quanto menor, melhor =========================================

rmse(test$fert2005, output4)
rmse(test$fert2005, output5)
rmse(test$fert2005, output6)

