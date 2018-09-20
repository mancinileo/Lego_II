##================================
##
##   Script aula 6.2
##
##================================

rm(list = ls())

## abrir pacotes ===================================

pacman::p_load(tidyverse, janitor, 
               sjPlot, rio, foreign)

## abrir base ======================================

base_idh <- read.dta("IDH_2007.dta")

## nomes da base

names(base_idh)

## selecionar variaveis ============================

base_idh2 <- base_idh %>% 
  select(fert2005, imr2005)

## sumarizar variaveis 

summary(base_idh2)

## filtrar casos extremos que sao NAs 

base_idh2 <- base_idh2 %>% 
  filter(fert2005 != 999 & imr2005 != 999) 


base_idh2 %>% 
  summary(.)


## modelo sem transformacoes =====================

modelo1 <- lm(imr2005 ~ fert2005, 
              data = base_idh2)

summary(modelo1)

## diagnosticos 

library(sjPlot)

plot_model(modelo1, type = "diag")

## correção nos resíduos ========================

library(lmtest)
library(car)

cov1 <- hccm(modelo1, type="hc1") 
modelo_het <- coeftest(modelo1, vcov.=cov1)

modelo_het

## modelo com WLS ================================

modelo2 <- lm(imr2005 ~ fert2005, 
              weight=1/fert2005^2,
              data = base_idh2)

summary(modelo2)

## violacoes continuam as mesmas

plot_model(modelo2, type = "diag")

## ver magnitude dos efeitos

plot_model(modelo2, type = "pred", 
           terms = "fert2005") +
  theme_bw()


