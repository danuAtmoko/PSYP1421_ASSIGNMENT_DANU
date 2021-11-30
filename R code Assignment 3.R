#start

library(tidyverse)
library(psych)
library(cAIC4) 
library(r2glmm) 
library(lme4) 
library(lmerTest)
library(MuMIn)


# code below from https://stackoverflow.com/questions/25142901/standardized-coefficients-for-lmer-model
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}

data_file_3 = read.csv("https://tinyurl.com/b385chpu")
data_file_4 = read.csv("https://tinyurl.com/4f8thztv")

View(data_file_3)
View(data_file_4)
str(data_file_3)
str(data_file_4)
colSums(is.na(data_file_3))
colSums(is.na(data_file_4))
summary(data_file_3)
summary(data_file_4)


data_file_3 = data_file_3 %>% 	
  mutate(hospital = factor(hospital))

data_file_3 %>% 		
  ggplot() +		
  aes(y = pain, x = age) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)		

data_file_3 %>% 		
  ggplot() +		
  aes(y = pain, x = pain_cat) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)

data_file_3 %>% 		
  ggplot() +		
  aes(y = pain, x = sex) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)

data_file_3 <- data_file_3 %>%
  mutate(sex=replace(sex, sex=="woman","female"))

data_file_3 %>% 		
  ggplot() +		
  aes(y = pain, x = sex) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)

data_file_3 %>% 		
  ggplot() +		
  aes(y = pain, x = mindfulness) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)

data_file_3 %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)

data_file_3 %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_saliva) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)


int_plot = data_file_3 %>% 		
  ggplot() +		
  aes(y = pain, x = pain_cat, color = hospital) +		
  geom_point(size = 4) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE)	


int_plot     #intercept plot for pain_cat



#model fixed intercept
model_fix_int <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum +
                cortisol_saliva + mindfulness, data = data_file_3)

model_fix_int

#random intercept model
mod_ran_int = lmer(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum +
                     cortisol_saliva + mindfulness + 
                       (1|hospital), data = data_file_3)

mod_ran_int

summary(mod_ran_int)
plot(mod_ran_int)


tableConfin<-confint(mod_ran_int)
tableConfin
tableCoef <-stdCoef.merMod(mod_ran_int)
tableCoef

write.table(tableCoef, file = "tablestdCoef.txt", sep = ",", quote = FALSE, row.names = F)
write.table(tableConfin, file = "tablesconfin.txt", sep = ",", quote = FALSE, row.names = F)

#mixed model only the most influential predictor
model_mixed = lmer(pain ~ cortisol_serum +
                     (cortisol_serum|hospital), data = data_file_3)

summary(model_mixed)


sum(residuals(model_fix_int)^2) #RSS regular linear regression

sum(residuals(mod_ran_int)^2)   #RSS random intercept

sum(residuals(model_mixed)^2)   #RSS random slope / mixed model

cAIC(model_fix_int)$caic  #common linier regreesion
cAIC(mod_ran_int)$caic  #random intercept
cAIC(model_mixed)$caic  #random slope/mixed model

anova(mod_ran_int, model_mixed)


# marginal and conditional R squared values #from data 3
r.squaredGLMM(mod_ran_int)

# Marginal R squared #for data 4
r2beta(mod_ran_int, method = "nsj", data = data_file_4)

#prediction equation
pred_Ran_Int <- predict(mod_ran_int, data_file_4, allow.new.levels=T)

random_inter_pred = cbind(data_file_4, pred_Ran_Int)
View(random_inter_pred)

RSS = sum((data_file_4$pain - pred_Ran_Int)^2)
RSS

mod_mean <- lmer(pain ~ 1 + (1|hospital), data = data_file_4)
TSS = sum((data_file_4$pain - predict(mod_mean))^2)
TSS

RSquared_data4 <- 1-(RSS/TSS)
RSquared_data4

summary(pred_Ran_Int)

#visulization
data_file_3 = data_file_3 %>%
  mutate(pred_int = predict(mod_ran_int), pred_slope = predict(model_mixed))

#random intercept model
data_file_3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + geom_line(color = "red",
                     aes(y = pred_int, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)

#random slope model
data_file_3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + geom_line(color = "red",
  aes(y = pred_slope, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)


#end 
#danuLundUniversity2021Assignment#