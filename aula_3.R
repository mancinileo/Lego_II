##=============================================
##   Aula 13 - Regressão Multivariada
##=============================================

rm(list = ls())

library(tidyverse)
library(janitor)
library(ggfortify)
library(car)

data("Duncan")

## saber nomes das variáveis =================

names(Duncan)

## saber tipos das variáveis =================

glimpse(Duncan)

## sumário das variáveis =====================

summary(Duncan)

## correlação das variáveis contínuas ========

Duncan %>% 
  select(prestige, income, education) %>% 
  cor(.)

## Visualizar a associação entre as duas =====

Duncan %>% 
  ggplot(aes(x = prestige, y = income)) + 
  geom_point()


## Visualizar a associação entre as duas =====

Duncan %>% 
  ggplot(aes(x = prestige, y = income)) + 
  geom_point() + 
  geom_smooth(method = "lm")




## regressões ================================

## modelo 1: prestígio x renda

modelo1 <- lm(prestige ~ income, 
              data = Duncan)

summary(modelo1)

options(scipen = 999)

## modelo 2: prestígio = renda + educação

modelo2 <- lm(prestige ~ income + education, 
              data = Duncan)

summary(modelo2) 


## verificar multicolinearidade ============

vif(modelo2)

## premissas do modelo =====================

## linearidade

plot(modelo2)

autoplot(modelo2)

## normalidade

hist(rstudent(modelo2), nclass=12)

## ou... do jeito moderno!!

library(broom)
df <- augment(modelo2)

df %>% 
  ggplot(aes(.resid)) +
  geom_histogram(bins = 12)
  
df %>% 
  ggplot(aes(.resid)) +
  geom_density()


## Truques =================================

library(sjPlot) # bayesiano mara!

## Predição de categóricas

plot_model(modelo2, type = "pred", 
           terms = "education")

teste <- plot_model(modelo2, type = "pred", 
           terms = "income")

teste + theme_bw()

plot_model(modelo2, type = "slope")

plot_model(modelo2, type = "resid")

plot_model(modelo2, type = "diag")

library(stargazer)

stargazer(modelo1, out = "teste.html")
















