##=========================================
##
##           Aula 6 - Lego II
##
##=========================================

rm(list = ls())
options(scipen = 999)

library(tidyverse)
library(haven)
library(foreign)
library(lmtest)
library(sjPlot)
library(rio)
dir()

base_idh <- import("IDH_2007.dta")

names(base_idh)

## selecionar somente 3 variáveis
base_idh2 <- base_idh %>% 
  select(fert2005, gdpcap05, tempera)


## olhar tipo 
glimpse(base_idh2)


## ver sumário 
summary(base_idh2)


## retirar os NAs, caracterizados como 999 e 999999
base_idh2 <- base_idh2 %>% 
  filter(fert2005 != 999 & gdpcap05 != 999999) 


## olhar a distribuição da variável dependente
base_idh2 %>% 
  ggplot(aes(fert2005)) + 
  geom_density()


## olhar a distribuição da variável independente
base_idh2 %>% 
  ggplot(aes(gdpcap05)) + 
  geom_density()

## tabular segunda variável independente
base_idh2 %>% 
  tabyl(tempera)



## ==================================================
##       Modelos de Regressão Linear 
## ==================================================

## modelo 1: fertilidade e gdp cap ==================

modelo1 <- lm(fert2005 ~ gdpcap05, 
              data = base_idh2)

summary(modelo1)

## teste de durbin watson para saber se os erros são 
## independentes (R: sim, são)
dwtest(modelo1)

## olhar distribuição dos erros (testar para ver se é normal)
hist(rstudent(modelo1), nclass=12)

## olhar dignósticos do modelo (problemas de normalidade e 
## linearidade)
library(ggfortify)
autoplot(modelo1)


## scatterplot para ver a associação entre variáveis 
## dependente e independente (novamente, não linearidade)

base_idh2 %>% 
  ggplot(aes(x = gdpcap05, y = fert2005)) + 
  geom_point() + 
  geom_smooth(method = "lm")


## diagnósticos mais completos com o sjplot

library(sjPlot)

plot_model(modelo1, type = "diag")

## ================================================


## Modelo 2: fertilidade, gdp e temperatura =========


modelo2 <- lm(fert2005 ~ gdpcap05 + tempera, 
              data = base_idh2)

summary(modelo2)

## problema: tempera é variável categórica e consta 
## como double na base

glimpse(base_idh2)

## problema corrigido
modelo2 <- lm(fert2005 ~ gdpcap05 + factor(tempera), 
              data = base_idh2)

summary(modelo2)

## diagnóstico completo do modelo
plot_model(modelo2, type = "diag")

## ================================================

## Modelo 3: interação entre temperatura e gdp =======

## transformar tempera em categórica logo na base

base_idh2 <- base_idh2 %>% 
  mutate(tempera = factor(tempera))


modelo3 <- lm(fert2005 ~ gdpcap05*tempera, 
              data = base_idh2)

summary(modelo3)

## gráfico com efeito da interação entre gdp e tempera

plot_model(modelo3, type = "pred", 
           terms = c("gdpcap05", "tempera"))

## repare que efeito de gdp diminui quando interage 
## com a variável tempera. 











