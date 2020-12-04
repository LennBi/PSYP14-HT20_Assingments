# Home assignment for the 2020 autumn semester - PSYP14-HT20

# Assignment 2:
# Contrast the performance of the theory based model with that of a
# model determined by an automated model selection approach

# The following packages were used:

library(tidyverse)
library(psych)
library(gridExtra)
library(lm.beta)
library(car)
library(lmtest)

# The following custom functions were used:

coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	

# Retrieving the dataset and preparing it for use, according to assignment 1:

pain_data_original <- read.csv("https://tinyurl.com/ha-dataset1")

pain_data_corrected <- pain_data_original %>%
  slice(-c(93, 150))

pain_data_corrected <- pain_data_corrected %>%
  mutate(sex = factor(sex))

# Data exploration and descriptive statistics:

# Exploring weight

pain_data_corrected %>%
  select(weight) %>%
  describe()

histogram_weight <- pain_data_corrected %>%
  ggplot() +
  aes(x = weight) +
  geom_histogram(binwidth = 1)

plot_weight_pain <- pain_data_corrected %>%
  ggplot() +
  aes(x = weight,
      y = pain) +
  geom_point() +
  geom_smooth()

grid.arrange(histogram_weight, plot_weight_pain, nrow = 1)

# Exploring IQ

pain_data_corrected %>%
  select(IQ) %>%
  describe()

histogram_IQ <- pain_data_corrected %>%
  ggplot() +
  aes(x = IQ) +
  geom_histogram(binwidth = 10)

plot_IQ_pain <- pain_data_corrected %>%
  ggplot() +
  aes(x = IQ,
      y = pain) +
  geom_point() +
  geom_smooth()

grid.arrange(histogram_IQ, plot_IQ_pain, nrow = 1)

# Exploring household_income

pain_data_corrected %>%
  select(household_income) %>%
  describe()

histogram_household_income <- pain_data_corrected %>%
  ggplot() +
  aes(x = household_income) +
  geom_histogram(binwidth = 1000)

plot_household_income_pain <- pain_data_corrected %>%
  ggplot() +
  aes(x = household_income,
      y = pain) +
  geom_point() +
  geom_smooth()

grid.arrange(histogram_household_income, plot_household_income_pain, nrow = 1)

# Histogram overview

histogram_comparison2 <- grid.arrange(histogram_weight, histogram_IQ, histogram_household_income) 

histogram_comparison2

# Explore the correlations between all continuous variables:

table_correlations <- pain_data_corrected %>%
  select(-ID, -sex) %>%
  drop_na() %>%
  cor()  

view(table_correlations)

# Creating the backwards regression:

# Creating the initial model with all predictors included

initial_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + IQ + household_income, data = pain_data_corrected)

initial_model

summary(initial_model)

# Creating a model by using backwards regression

backward_model = step(initial_model, direction = "backward")

backward_model

# Recreating the model 2 out of assignment 1:

mod2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = pain_data_corrected)

theory_based_model = mod2 

# Looking at Leverage and Cook's distance

backward_model %>%
  plot(which = 5)

backward_model %>%
  plot(which = 4)

# 3 values load high on Leverage and Cook's distance. Let's look at those:

pain_data_corrected %>%
  slice(c(102, 113, 147))

# All 3 values seem to be not the result of an error and therefore valid data
# Despite having a Cook's distance higher than 4/n, we will not exclude data at this point

# Checking the assumption for the backward_model:

# Assumptions
#   Normality
#   Linearity
#   Homoscedasticity
#   No Multicolinearity

# Checking the backward_model for Normality by QQ-PLot

backward_model %>%
  plot(which = 2)

residuals_backward_model <- enframe(residuals(backward_model))
residuals_backward_model %>%
  ggplot() +
  aes(x = value) +
  geom_histogram()

describe(residuals(backward_model))

# Since the QQ-Plot seems to only include some minor curvatures and the skewness
# and kurtosis lie between -1 and 1, we can accept the assumption of Normality

# Checking backward_model for Linearity

backward_model %>%
  residualPlots()

# Even though there are minor curvatures visible on the plots, the tests are all
# non significant, so we accept the linearity assumption

# Checking backward_model for Homoscedasticity

backward_model %>%
  plot(which = 3)

backward_model %>%
  ncvTest()

backward_model %>%
  bptest()

# Since we cannot spot a funnel-shape in the plot and there are no significant
# test results of the NCV and the Breusch-Pagan test, we can accept the 
# assumption of Homoscedasticity.

# Checking backward_model for No Multicolinearity

backward_model %>%
  vif()

# The results of the VIF suggest no violation of the assumption of Multicolinearity

# All assumptions are met.

# Backward Model 

summary(backward_model)
AIC(backward_model)

# The multiple regression model was significantly better than the null model,
# explaining 48.12% of the variance in perceived pain (F(6,151) = 25.27, p < 0.001,
# Adj. R^2 = 0.48, AIC = 494.76)

# Regression equation: f(x) = 1.95 + (-0.04*x1) + (0.28*x2) + (0.11*x3) + (0.52*x4) + (-0.26*x5) + ((-6.5*e-06)*x5)

coef_table(backward_model)

# Model Comparison between initial model and backwards model:

# Comparing two models via adj. R^2

summary(initial_model)$adj.r.squared
summary(backward_model)$adj.r.squared

# The variance explained by the backwards model is higher than the
# variance exlpained by the initial model.

# Comparing two models via AIC

AIC(initial_model)
AIC(backward_model)

# AIC(initial_model) = 499.8 > AIC(backward_model) = 494.76; significant difference since 2 points difference

# Comparing the models based on their residual error and degrees of freedom by using an ANOVA.
# Possible since the backward_model is nested within the initial model

anova(backward_model, initial_model)

# The ANOVA shows, that the backward_model is not significantly better than the initial_model.

# Model Comparison between the theory_based_model and the backward_model

# Comparing two models via adj. R^2

summary(theory_based_model)$adj.r.squared
summary(backward_model)$adj.r.squared

# The variance explained has increased substantially by adding more information
# to the model

# Comparing two models via AIC

AIC(theory_based_model)
AIC(backward_model)

# AIC(theory_based_model) = 497.06 > AIC(backward_model) = 494.76; significant difference since 2 points difference

# No ANOVA is possible, since the models are not nested within each other.

# Prediction:

# Compare the predicted values with the actual pain ratings. 
# Which model was able to predict the actual pain ratings 
# in data file 2 better?

# Retrieving dataset:

pain_data_prediction <- read.csv("https://tinyurl.com/ha-dataset2")

# Data overview:

view(pain_data_prediction)
summary(pain_data_prediction)
nrow(pain_data_prediction)
ncol(pain_data_prediction)

# Pain: Scale of 1 to 10; The higher the more painful
# Sex: Factor of 2 sexes; Levels: female and male
# STAI-trait: Scale of 20 to 80; Higher scores mean higher anxiety
# Pain catastrophizing: Score from 0 to 52; the higher the score, the higher catastrophizing
# Mindfulness: Score from 1 to 6; The higher the score, the higher dispositional mindfulness
# Cortisol levels positively associated with pain experience

# Identified incorrect value, ID = 8, Mindfulness = 7.17

# Since we do not know the individual test subjects, we cannot guarantee that the incorrect
# values are only typos. To be conservative here and due to the size of our dataset, 
# we will exclude ID = 8:

pain_data_prediction_corrected <- pain_data_prediction %>%
  slice(-c(8))

# Before exploration, translation of "sex" into a factor

pain_data_prediction_corrected = pain_data_prediction_corrected %>%
  mutate(sex = factor(sex))

# Data exploration and descriptive statistics ("short version"):

# Exploring pain:

pain_data_prediction_corrected %>%
  select(pain) %>%
  describe()

pain_data_prediction_corrected %>%
  ggplot() +
  aes(x = pain) +
  geom_histogram()

# Exploring sex:

pain_data_prediction_corrected %>%
  select(sex) %>%
  summary()

pain_data_prediction_corrected %>%
  ggplot() +
  aes(x = sex) +
  geom_bar()

# Exploring age:

pain_data_prediction_corrected %>%
  select(age) %>%
  describe()

pain_data_prediction_corrected %>%
  ggplot() +
  aes(x = age) +
  geom_histogram()

# Exploring STAI_trait:

pain_data_prediction_corrected %>%
  select(STAI_trait) %>%
  describe()

pain_data_prediction_corrected %>%
  ggplot() +
  aes(x = STAI_trait) +
  geom_histogram()

# Exploring pain_cat:

pain_data_prediction_corrected %>%
  select(pain_cat) %>%
  describe()

pain_data_prediction_corrected %>%
  ggplot() +
  aes(x = pain_cat) +
  geom_histogram()

# Exploring cortisol_serum:

pain_data_prediction_corrected %>%
  select(cortisol_serum) %>%
  describe()

pain_data_prediction_corrected %>%
  ggplot() +
  aes(x = cortisol_serum) +
  geom_histogram()

# Exploring mindfulness:

pain_data_prediction_corrected %>%
  select(mindfulness) %>%
  describe()

pain_data_prediction_corrected %>%
  ggplot() +
  aes(x = mindfulness) +
  geom_histogram()

# Exploring weight:

pain_data_prediction_corrected %>%
  select(weight) %>%
  describe()

pain_data_prediction_corrected %>%
  ggplot() +
  aes(x = weight) +
  geom_histogram()

# Exploring IQ:

pain_data_prediction_corrected %>%
  select(IQ) %>%
  describe()

pain_data_prediction_corrected %>%
  ggplot() +
  aes(x = IQ) +
  geom_histogram(binwidth = 1)

# Exploring household_income:

pain_data_prediction_corrected %>%
  select(household_income) %>%
  describe()

pain_data_prediction_corrected %>%
  ggplot() +
  aes(x = household_income) +
  geom_histogram(binwidth = 1000)

# Prediction:

# Predict data: theory-based model (tbm)

predictions_tbm <- predict(theory_based_model, newdata = pain_data_prediction_corrected)
view(predictions_tbm)

theory_based_data_prediction <- pain_data_prediction_corrected %>% 
  select(-c(cortisol_saliva, weight, IQ, household_income))

predict_with_tbm <- cbind(predictions_tbm, theory_based_data_prediction)

view(predict_with_tbm)

# Predict data: backwards regression

predictions_brm = predict(backward_model, newdata = pain_data_prediction_corrected)
view(predictions_brm)

backward_data_prediction <- pain_data_prediction_corrected %>% 
  select(-c(STAI_trait, cortisol_saliva, weight, IQ))

predict_with_brm <- cbind(predictions_brm, backward_data_prediction)

view(predict_with_brm)

# Comparing the prediction of both models:

RSS_theory_based = sum((pain_data_prediction_corrected$pain - predictions_tbm)^2)

RSS_backward = sum((pain_data_prediction_corrected$pain - predictions_brm)^2)