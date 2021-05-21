library(tidyverse)
library(NHANES)
library(MASS)
library(car)
library(performance)

NHANES_tidied <- NHANES %>% 
  distinct(ID, .keep_all = TRUE)

NHANES_tidied <- NHANES_tidied %>% 
  filter(!is.na(BMI) & !is.na(AgeMonths) & !is.na(Poverty) & !is.na(Pulse) & !is.na(BPSysAve))

model0 <-  lm(TotChol ~ 1, data = NHANES_tidied) 
model1 <- lm(TotChol ~ BMI + AgeMonths + Poverty + Pulse + BPSysAve, data = NHANES_tidied)
summary(model1)

vif(model1)

check_model(model1)

steplimitsf <- step(model0, scope = list (lower = model0, upper = model1), direction = "forward")
summary(steplimitsf)

model_final <- lm(TotChol ~ BMI + AgeMonths + Pulse + BPSysAve, data = NHANES_tidied)
anova(model1, model_final)            

model_no_BMI <- lm(TotChol ~ AgeMonths + Pulse + BPSysAve, data = NHANES_tidied)
anova(model_final, model_no_BMI)
