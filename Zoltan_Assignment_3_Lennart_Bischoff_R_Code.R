# Home assignment for the 2020 autumn semester - PSYP14-HT20

# Regression models with fixed and random effects

# Assignment 3:
# Build the same model on data originating from different data collection
# sites

# The following packages were used:

library(tidyverse)
library(psych)
library(gridExtra)
library(lm.beta)
library(car)
library(lmtest)
library(boot)
library(lme4)
library(cAIC4) 
library(r2glmm)
library(lmerTest)
library(optimx)
library(MuMIn)

# The following custom functions were used:

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

# Retrieving the datasets:

dataset3 <- read.csv("https://tinyurl.com/ha-dataset3")

dataset4 <- read.csv("https://tinyurl.com/ha-dataset4")

# Dataset 3 overview:

view(dataset3)
summary(dataset3)
nrow(dataset3)
ncol(dataset3)

# Dataset 4 overview:

view(dataset4)
summary(dataset4)
nrow(dataset4)
ncol(dataset4)

# Pain: Scale of 1 to 10; The higher the more painful
# Sex: Factor of 2 sexes; Levels: female and male
# STAI-trait: Scale of 20 to 80; Higher scores mean higher anxiety
# Pain catastrophizing: Score from 0 to 52; the higher the score, the higher catastrophizing
# Mindfulness: Score from 1 to 6; The higher the score, the higher dispositional mindfulness
# Cortisol levels positively associated with pain experience

# Identified incorrect value; dataset4, ID = 80, mindfulness = 6.05
# Identified incorrect value; dataset3, ID = 182, sex = femlae

# Since we do not know the individual test subjects, we cannot guarantee that the incorrect
# values are only typos. To be conservative here and due to the size of our dataset, 
# we will exclude ID = 80:

dataset4 = dataset4 %>%
  slice(-(80))

# Since we can argue that "femlae" is a clear typo, we will correct the answer

dataset3["182", "sex"] <- "female"

# Before exploration, translation of "sex" and "hospital" into a factor

dataset3 = dataset3 %>%
  mutate(sex = factor(sex))

dataset4 = dataset4 %>%
  mutate(sex = factor(sex))

dataset3 = dataset3 %>%
  mutate(hospital = factor(hospital))

dataset4 = dataset4 %>%
  mutate(hospital = factor(hospital))

# Data exploration and descriptive statistics:

# dataset3

# Exploring pain:

dataset3 %>%
  select(pain) %>%
  describe()

histogram_pain3 <- dataset3 %>%
  ggplot() +
  aes(x = pain) +
  geom_bar()

# Exploring sex:

dataset3 %>%
  select(sex) %>%
  table()

histogram_sex3 <- dataset3 %>%
  ggplot() +
  aes(x = sex) +
  geom_bar()

# Exploring the connection between sex and pain:

plot_sex_pain3 <- dataset3 %>%
  ggplot() +
  aes(x = sex,
      y = pain) +
  geom_boxplot()

plot_sex_pain3.2 <- dataset3 %>%
  ggplot() +
  aes(x = sex,
      y = pain) +
  geom_violin() +
  geom_jitter(width = 0.1)

plot_sex_pain3.3 <- dataset3 %>%
  ggplot() +
  aes(x = pain,
      fill = sex) +
  geom_density(alpha = 0.3) + 
  theme(legend.position="top")

grid.arrange(plot_sex_pain3, plot_sex_pain3.2, plot_sex_pain3.3, nrow = 1)

# Exploring age:

dataset3 %>%
  select(age) %>%
  describe()

histogram_age3 <- dataset3 %>%
  ggplot() +
  aes(x = age) +
  geom_bar()

plot_age_pain3 <- dataset3 %>%
  ggplot() +
  aes(x = age,
      y = pain) +
  geom_point() +
  geom_smooth()

grid.arrange(histogram_age3, plot_age_pain3, nrow = 1)

# Exploring STAI_trait:

dataset3 %>%
  select(STAI_trait) %>%
  describe()

histogram_STAI3 <- dataset3 %>%
  ggplot() +
  aes(x = STAI_trait) +
  geom_bar()

plot_STAI_pain3 <- dataset3 %>%
  ggplot() +
  aes(x = STAI_trait,
      y = pain) +
  geom_point() +
  geom_smooth()

grid.arrange(histogram_STAI3, plot_STAI_pain3, nrow = 1)

# Exploring pain catastrophizing:

dataset3 %>%
  select(pain_cat) %>%
  describe()

histogram_pain_cat3 <- dataset3 %>%
  ggplot() +
  aes(x = pain_cat) +
  geom_bar()

plot_pain_cat_pain3 <- dataset3 %>%
  ggplot() +
  aes(x = pain_cat,
      y = pain) +
  geom_point() +
  geom_smooth()

grid.arrange(histogram_pain_cat3, plot_pain_cat_pain3, nrow = 1)

# Exploring mindfulness:

dataset3 %>%
  select(mindfulness) %>%
  describe()

histogram_mindfulness3 <- dataset3 %>%
  ggplot() +
  aes(x = mindfulness) +
  geom_bar()

plot_mindfulness_pain3 <- dataset3 %>%
  ggplot() +
  aes(x = mindfulness,
      y = pain) +
  geom_point() +
  geom_smooth()

grid.arrange(histogram_mindfulness3, plot_mindfulness_pain3, nrow = 1)

# Exploring Cortisol_serum:

dataset3 %>%
  select(cortisol_serum) %>%
  describe()

histogram_cortisol_serum3 <- dataset3 %>%
  ggplot() +
  aes(x = cortisol_serum) +
  geom_bar()

plot_cortisol_serum_pain3 <- dataset3 %>%
  ggplot() +
  aes(x = cortisol_serum,
      y = pain) +
  geom_point() +
  geom_smooth()

grid.arrange(histogram_cortisol_serum3, plot_cortisol_serum_pain3, nrow = 1)

# Exploring Cortisol_saliva:

dataset3 %>%
  select(cortisol_saliva) %>%
  describe()

histogram_cortisol_saliva3 <- dataset3 %>%
  ggplot() +
  aes(x = cortisol_saliva) +
  geom_bar()

plot_cortisol_saliva3 <- dataset3 %>%
  ggplot() +
  aes(x = cortisol_saliva,
      y = pain) +
  geom_point() +
  geom_smooth()

grid.arrange(histogram_cortisol_saliva3, plot_cortisol_saliva3, nrow = 1)

# Explore the distribution of all the variables:

histogram_comparison3 <- grid.arrange(histogram_pain3, histogram_sex3, histogram_age3, histogram_STAI3, histogram_pain_cat3, histogram_mindfulness3, histogram_cortisol_serum3, histogram_cortisol_saliva3) 

# Explore the correlations between all continuous variables:

table_correlations3 <- dataset3 %>%
  select(-ID, -sex, -hospital) %>%
  drop_na() %>%
  cor()  

view(table_correlations3)

# dataset4

# Exploring pain:

dataset4 %>%
  select(pain) %>%
  describe()

histogram_pain4 <- dataset4 %>%
  ggplot() +
  aes(x = pain) +
  geom_bar()

# Exploring sex:

dataset4 %>%
  select(sex) %>%
  table()

histogram_sex4 <- dataset4 %>%
  ggplot() +
  aes(x = sex) +
  geom_bar()

# Exploring the connection between sex and pain:

plot_sex_pain4 <- dataset4 %>%
  ggplot() +
  aes(x = sex,
      y = pain) +
  geom_boxplot()

plot_sex_pain4.2 <- dataset4 %>%
  ggplot() +
  aes(x = sex,
      y = pain) +
  geom_violin() +
  geom_jitter(width = 0.1)

plot_sex_pain4.3 <- dataset4 %>%
  ggplot() +
  aes(x = pain,
      fill = sex) +
  geom_density(alpha = 0.3) + 
  theme(legend.position="top")

grid.arrange(plot_sex_pain4, plot_sex_pain4.2, plot_sex_pain4.3, nrow = 1)

# Exploring age:

dataset4 %>%
  select(age) %>%
  describe()

histogram_age4 <- dataset4 %>%
  ggplot() +
  aes(x = age) +
  geom_bar()

plot_age_pain4 <- dataset4 %>%
  ggplot() +
  aes(x = age,
      y = pain) +
  geom_point() +
  geom_smooth()

grid.arrange(histogram_age4, plot_age_pain4, nrow = 1)

# Exploring STAI_trait:

dataset4 %>%
  select(STAI_trait) %>%
  describe()

histogram_STAI4 <- dataset4 %>%
  ggplot() +
  aes(x = STAI_trait) +
  geom_bar()

plot_STAI_pain4 <- dataset4 %>%
  ggplot() +
  aes(x = STAI_trait,
      y = pain) +
  geom_point() +
  geom_smooth()

grid.arrange(histogram_STAI4, plot_STAI_pain4, nrow = 1)

# Exploring pain catastrophizing:

dataset4 %>%
  select(pain_cat) %>%
  describe()

histogram_pain_cat4 <- dataset4 %>%
  ggplot() +
  aes(x = pain_cat) +
  geom_bar()

plot_pain_cat_pain4 <- dataset4 %>%
  ggplot() +
  aes(x = pain_cat,
      y = pain) +
  geom_point() +
  geom_smooth()

grid.arrange(histogram_pain_cat4, plot_pain_cat_pain4, nrow = 1)

# Exploring mindfulness:

dataset4 %>%
  select(mindfulness) %>%
  describe()

histogram_mindfulness4 <- dataset4 %>%
  ggplot() +
  aes(x = mindfulness) +
  geom_bar()

plot_mindfulness_pain4 <- dataset4 %>%
  ggplot() +
  aes(x = mindfulness,
      y = pain) +
  geom_point() +
  geom_smooth()

grid.arrange(histogram_mindfulness4, plot_mindfulness_pain4, nrow = 1)

# Exploring Cortisol_serum:

dataset4 %>%
  select(cortisol_serum) %>%
  describe()

histogram_cortisol_serum4 <- dataset4 %>%
  ggplot() +
  aes(x = cortisol_serum) +
  geom_bar()

plot_cortisol_serum_pain4 <- dataset4 %>%
  ggplot() +
  aes(x = cortisol_serum,
      y = pain) +
  geom_point() +
  geom_smooth()

grid.arrange(histogram_cortisol_serum4, plot_cortisol_serum_pain4, nrow = 1)

# Exploring Cortisol_saliva:

dataset4 %>%
  select(cortisol_saliva) %>%
  describe()

histogram_cortisol_saliva4 <- dataset4 %>%
  ggplot() +
  aes(x = cortisol_saliva) +
  geom_bar()

plot_cortisol_saliva4 <- dataset4 %>%
  ggplot() +
  aes(x = cortisol_saliva,
      y = pain) +
  geom_point() +
  geom_smooth()

grid.arrange(histogram_cortisol_saliva4, plot_cortisol_saliva4, nrow = 1)

# Explore the distribution of all the variables:

histogram_comparison4 <- grid.arrange(histogram_pain4, histogram_sex4, histogram_age4, histogram_STAI4, histogram_pain_cat4, histogram_mindfulness4, histogram_cortisol_serum4, histogram_cortisol_saliva4) 

# Explore the correlations between all continuous variables:

table_correlations4 <- dataset4 %>%
  select(-ID, -sex, -hospital) %>%
  drop_na() %>%
  cor()  

view(table_correlations4)

# Building a linear mixed model on data 3, accounting for the clustering of the 
# data at different hospital sites, random intercept model including
# the random intercept of hospital-ID and the fixed effect predictors used in 
# assignment 1

model3 = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = dataset3)

# Note model coefficients and confidence intervals of coefficients for all
# fixed effect predictors and compare them to the ones obtained in assignment 1

summary(model3)
confint(model3)
AIC(model3)
cAIC(model3)
stdCoef.merMod(model3)

# Compute the variance explained by the fixed effect predictors using marginal R^2 
# Variance explained by fixed and random effect terms combined using 
# conditional R^2 

# R^2 beta with confidence intervals:
r2beta(model3, method = "nsj", data = dataset3)

# Marginal and conditional R squared values:
r.squaredGLMM(model3)

# Now use the regression equation obtained on data file 3 to predict pain in data 
# file 4, use regression equation derived based on data 3

predicted_data4 = predict(model3, newdata = dataset4, allow.new.levels = TRUE)

view(predicted_data4)

# Compute variance explained by the model on data file 4
# using the formula we learned in class: 1-(RSS/TSS)
# Compare this R^2 to marginal and conditional R^2 values for the model on data file 3

# RSS:
RSS4 <- sum((dataset4$pain - predicted_data4)^2)

# TSS:
mod_mean4 <-lm(pain ~ 1, data = dataset4)

TSS4 <- sum((dataset4$pain - predict(mod_mean4))^2)

# Variance R^2:
1-(RSS4/TSS4)

# Build a new linear mixed effects model on data3 predicting pain
# only include most influential predictor from previous model
# Allow for both random intercept and random slope

# Standardized beta for each predictor:

stdCoef.merMod(model3)

# We identified the predictor "cortisol_serum" as the most influential predictor

model4 <- lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = dataset3)

# Visualize the fitted regression lines for each hospital separately

dataset3_pred = dataset3 %>% 
  mutate(pred_slope = predict(model4))

dataset3_pred %>%
  ggplot() +
  aes(y = pain, x = cortisol_serum, group = hospital)+
  geom_point(aes(color = hospital), size = 4, col = "#6E6E6E") +
  geom_line(color='black', aes(y=pred_slope, x=cortisol_serum))+
  facet_wrap( ~ hospital, nrow = 2)


