#Start Here


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

data_No_outliers <- dataReady %>% 	
  slice(-c(74))

data_No_outliers %>%
  ggplot() + aes(x = age, y = pain) + geom_point() +
  geom_smooth(method = "lm")

data_No_outliers %>%
  ggplot() + aes(x = sex, y = pain) + geom_point() +
  geom_smooth(method = "lm")

data_No_outliers %>%
  ggplot() + aes(x = STAI_trait, y = pain) + geom_point() +
  geom_smooth(method = "lm")

data_No_outliers %>%
  ggplot() + aes(x = pain_cat, y = pain) + geom_point() +
  geom_smooth(method = "lm")

data_No_outliers %>%
  ggplot() + aes(x = cortisol_serum, y = pain) + geom_point() +
  geom_smooth(method = "lm")

data_No_outliers %>%
  ggplot() + aes(x = mindfulness, y = pain) + geom_point() +
  geom_smooth(method = "lm")

data_No_outliers %>%
  ggplot() + aes(x = IQ, y = pain) + geom_point() +
  geom_smooth(method = "lm")

data_No_outliers %>%
  ggplot() + aes(x = household_income, y = pain) + geom_point() +
  geom_smooth(method = "lm")

model_backward <- lm(pain ~ age + sex + STAI_trait + pain_cat +
                       cortisol_serum + mindfulness + 
                       weight + IQ + household_income, data = data_No_outliers) 
model_backward %>% 	      
  plot(which = 5)	

model_backward %>% 	     
  plot(which = 4)

data_No_outliers %>% 	
  slice(c(47, 84, 85))	

model_backward %>%  
  plot(which = 2)

model_backward_residual <- enframe(residuals(model_backward)) #normalitas histogram

model_backward_residual %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(model_backward_residual)

model_backward %>%       #linearity
  residualPlots()


model_backward %>%       #homoscedasticity test
  plot(which = 3)

model_backward %>%       #homoscedasticity test
  ncvTest()

model_backward %>%       #homoscedasticity test
  bptest()

model_backward %>%       #no multicollinearity test
  vif()

model_backward_full = step(model_backward, direction = "backward")


model_backward_final <- lm(pain ~ age + pain_cat +
                       cortisol_serum + mindfulness, data = data_No_outliers)

model_TheoryBased <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum +
                          cortisol_saliva + mindfulness, data = data_No_outliers)


anova(model_backward_final, model_TheoryBased)
anova(model_backward_final, model_backward)

summary(model_backward_final)
summary(model_TheoryBased)
summary(model_backward)

AIC(model_backward_final)
AIC(model_TheoryBased)
AIC(model_backward)

# calculate predicted values
pred_Theory <- predict(model_TheoryBased, data_No_outliers)
pred_Backward <- predict(model_backward_final, data_No_outliers)

# now we calculate the sum of squared residuals
RSS_test_theory = sum((data_No_outliers[, "pain"] - pred_Theory)^2)
RSS_test_back = sum((data_No_outliers[, "pain"] - pred_Backward)^2)

RSS_test_theory
RSS_test_back


table_Backward <- coef_table(model_backward_final)
table_Theory <- coef_table(model_TheoryBased)
table_Initial <- coef_table(model_backward)

table_Backward
table_Theory
table_Initial

write.table(table_Backward, file = "tablemodelbcakward.txt", sep = ",", quote = FALSE, row.names = F)
write.table(table_Theory, file = "tablemodelteori.txt", sep = ",", quote = FALSE, row.names = F)
write.table(table_Initial, file = "tableinitial.txt", sep = ",", quote = FALSE, row.names = F)


home_sample_2 = read.csv("https://tinyurl.com/87v6emky")
str(home_sample_2)
colSums(is.na(home_sample_2))
summary(home_sample_2)
View(home_sample_2)

home_sample_2 <- home_sample_2 %>% 	
  slice(-c(74))

Pain_predictions_1 <- predict(model_TheoryBased, home_sample_2)

Theorybased_Predict = cbind(home_sample_2, Pain_predictions_1)
View(Theorybased_Predict)

RSS = sum((home_sample_2$pain - predict(model_TheoryBased))^2)
RSS


Pain_predictions_2 <- predict(model_backward_final, home_sample_2)

Backwardmodel_Predict = cbind(home_sample_2, Pain_predictions_2)
View(Backwardmodel_Predict)

RSS = sum((home_sample_2$pain - predict(model_backward_final))^2)
RSS




#end