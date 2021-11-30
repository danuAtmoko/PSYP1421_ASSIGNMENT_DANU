#start here

library(tidyverse)
library(psych)
library(car)
library(lmtest)
library(ggplot2)
library(lm.beta)
library(broom)



#credit to zlotan for the code 
coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}





data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
str(data_sample_1)
data_sample_1$sex <- as.factor(data_sample_1$sex)
str(data_sample_1)
summary(data_sample_1$sex)
colSums(is.na(data_sample_1))
summary(data_sample_1)
data_sample_1$pain[data_sample_1$ID == "ID_88"] <- 5
data_sample_1$STAI_trait[data_sample_1$ID == "ID_34"] <- 42
dataReady<-data_sample_1
summary(dataReady)

dataReady %>%
  ggplot() +	
  aes(x = STAI_trait) +	
  geom_histogram()

dataReady %>% 	
  ggplot() +	
  aes(x = cortisol_saliva) +	
  geom_histogram()

model_1 <- lm(pain ~ age + sex, data = dataReady)

model_2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum +
                cortisol_saliva + mindfulness, data = dataReady) 

dataReady %>%
  ggplot() + aes(x = mindfulness, y = pain) + geom_point() +
  geom_smooth(method = "lm")

dataReady %>%
  ggplot() + aes(x = cortisol_saliva, y = pain) + geom_point() +
  geom_smooth(method = "lm")

dataReady %>%
  ggplot() + aes(x = age, y = pain) + geom_point() +
  geom_smooth(method = "lm")

dataReady %>%
  ggplot() + aes(x = sex, y = pain) + geom_point() +
  geom_smooth(method = "lm")

model_2 %>% 	      
  plot(which = 5)	

model_2 %>% 	     
  plot(which = 4)

dataReady %>% 	
  slice(c(47, 74, 86))	

model_2 %>%  
  plot(which = 2)

dataReady %>% 	
  slice(c(74, 85, 104))

model_2_residual <- enframe(residuals(model_2)) #normality histogram
model_2_residual %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(model_2_residual)


data_No_outliers <- dataReady %>% 	
  slice(-c(74))


model_1 <- lm(pain ~ age + sex, data = data_No_outliers)

model_2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum +
                cortisol_saliva + mindfulness, data = data_No_outliers)

model_2 %>% 	      
  plot(which = 5)	

model_2 %>% 	     #cook distance
  plot(which = 4)

model_2 %>%       #normality
  plot(which = 2)

model_2_residual <- enframe(residuals(model_2)) #normality histogram

model_2_residual %>%
  ggplot() + aes(x = value) + geom_histogram()  #normality histogram

describe(model_2_residual)                    #normality skewness & kurtosis

model_2 %>%       #linearity
  residualPlots()


model_2 %>%       #homoscedasticity test
  plot(which = 3)

model_2 %>%       #homoscedasticity test
  ncvTest()

model_2 %>%       #homoscedasticity test
  bptest()

model_2 %>%       #no multicollinearity test
  vif()

model_1
model_2

anova(model_1, model_2)

summary(model_1)
summary(model_2)

AIC(model_1)
AIC(model_2)

confint(model_1)
confint(model_2)

lm.beta(model_1)
lm.beta(model_2)


summary(model_1)$adj.r.squared
summary(model_2)$adj.r.squared

table1 <- coef_table(model_1)
table2 <- coef_table(model_2)

table1
table2

write.table(table1, file = "tablemodel1.txt", sep = ",", quote = FALSE, row.names = F)
write.table(table2, file = "tablemodel2.txt", sep = ",", quote = FALSE, row.names = F)



# end