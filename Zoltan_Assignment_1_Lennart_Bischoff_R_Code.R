# Home assignment for the 2020 autumn semester - PSYP14-HT20

# Regression models with fixed and random effects

# Assignment 1:
# Assess the added benefit of including some psychological and hormonal
# predictors to the already established demographic predictors of pain.
# Gradually set up a hierarchical regression model to predict postoperative
# pain after wisdom tooth surgery.

# Research problem:
# Amount of pain around and after surgeries are highly variable between
# and within individuals. What influences pain and predicts the amount of
# pain that will be experienced.

# Data:
# Related to assessing influence of trait and state psychological measures
# on pain, and to see weather taking into account these variables can
# improve our understanding of postoperative pain.

# Research question 1:
# Age and sex seem to be predictors of pain: 
#     - age negatively associated with pain
#     - sex as predictor dependent on type of the procedure
# Determine the extent to which taking account psychological and hormonal
# variables aside from used demographic variables improve our understanding
# of postoperative pain.

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

# Retrieving the dataset:

pain_data_original <- read.csv("https://tinyurl.com/ha-dataset1")

# Data overview:

view(pain_data_original)
summary(pain_data_original)
nrow(pain_data_original)
ncol(pain_data_original)

# Pain: Scale of 1 to 10; The higher the more painful
# Sex: Factor of 2 sexes; Levels: female and male
# STAI-trait: Scale of 20 to 80; Higher scores mean higher anxiety
# Pain catastrophizing: Score from 0 to 52; the higher the score, the higher catastrophizing
# Mindfulness: Score from 1 to 6; The higher the score, the higher dispositional mindfulness
# Cortisol levels positively associated with pain experience

# Identified incorrect value, ID = 93, age = 444
# Identified incorrect value, ID = 150, STAI_trait = 3.9

# Since we do not know the individual test subjects, we cannot guarantee that the incorrect
# values mentioned above are only typos or coding mistakes. 
# To assess the data more conservative and due to the size of the dataset, 
# we will therefore exclude ID = 93 and ID = 150:

pain_data_corrected <- pain_data_original %>%
  slice(-c(93, 150))

# Before exploration, translation of "sex" into a factor

pain_data_corrected <- pain_data_corrected %>%
  mutate(sex = factor(sex))

# Data exploration and descriptive statistics:

# Exploring pain:

pain_data_corrected %>%
  select(pain) %>%
  describe()

histogram_pain <- pain_data_corrected %>%
  ggplot() +
  aes(x = pain) +
  geom_histogram(binwidth = 1)

# Exploring sex:

pain_data_corrected %>%
  select(sex) %>%
  table()

histogram_sex <- pain_data_corrected %>%
  ggplot() +
  aes(x = sex) +
  geom_bar()

# Exploring the connection between sex and pain:

plot_sex_pain <- pain_data_corrected %>%
  ggplot() +
  aes(x = sex,
      y = pain) +
  geom_boxplot()

plot_sex_pain2 <- pain_data_corrected %>%
  ggplot() +
  aes(x = sex,
      y = pain) +
  geom_violin() +
  geom_jitter(width = 0.1)

plot_sex_pain3 <- pain_data_corrected %>%
  ggplot() +
  aes(x = pain,
      fill = sex) +
  geom_density(alpha = 0.3) + 
  theme(legend.position="top")

grid.arrange(plot_sex_pain, plot_sex_pain2, plot_sex_pain3, nrow = 1)

# Exploring age:

pain_data_corrected %>%
  select(age) %>%
  describe()

histogram_age <- pain_data_corrected %>%
  ggplot() +
  aes(x = age) +
  geom_histogram(binwidth = 1)

plot_age_pain <- pain_data_corrected %>%
  ggplot() +
  aes(x = age,
      y = pain) +
  geom_point() +
  geom_smooth()

grid.arrange(histogram_age, plot_age_pain, nrow = 1)

# Exploring STAI_trait:

pain_data_corrected %>%
  select(STAI_trait) %>%
  describe()

histogram_STAI <- pain_data_corrected %>%
  ggplot() +
  aes(x = STAI_trait) +
  geom_histogram(binwidth = 1)

plot_STAI_pain <- pain_data_corrected %>%
  ggplot() +
  aes(x = STAI_trait,
      y = pain) +
  geom_point() +
  geom_smooth()

grid.arrange(histogram_STAI, plot_STAI_pain, nrow = 1)

# Exploring pain catastrophizing:

pain_data_corrected %>%
  select(pain_cat) %>%
  describe()

histogram_pain_cat <- pain_data_corrected %>%
  ggplot() +
  aes(x = pain_cat) +
  geom_histogram(binwidth = 1)

plot_pain_cat_pain <- pain_data_corrected %>%
  ggplot() +
  aes(x = pain_cat,
      y = pain) +
  geom_point() +
  geom_smooth()

grid.arrange(histogram_pain_cat, plot_pain_cat_pain, nrow = 1)

# Exploring mindfulness:

pain_data_corrected %>%
  select(mindfulness) %>%
  describe()

histogram_mindfulness <- pain_data_corrected %>%
  ggplot() +
  aes(x = mindfulness) +
  geom_histogram()

plot_mindfulness_pain <- pain_data_corrected %>%
  ggplot() +
  aes(x = mindfulness,
      y = pain) +
  geom_point() +
  geom_smooth()

grid.arrange(histogram_mindfulness, plot_mindfulness_pain, nrow = 1)

# Exploring Cortisol_serum:

pain_data_corrected %>%
  select(cortisol_serum) %>%
  describe()

histogram_cortisol_serum <- pain_data_corrected %>%
  ggplot() +
  aes(x = cortisol_serum) +
  geom_histogram()

plot_cortisol_serum_pain <- pain_data_corrected %>%
  ggplot() +
  aes(x = cortisol_serum,
      y = pain) +
  geom_point() +
  geom_smooth()

grid.arrange(histogram_cortisol_serum, plot_cortisol_serum_pain, nrow = 1)

# Exploring Cortisol_saliva:

pain_data_corrected %>%
  select(cortisol_saliva) %>%
  describe()

histogram_cortisol_saliva <- pain_data_corrected %>%
  ggplot() +
  aes(x = cortisol_saliva) +
  geom_histogram()

plot_cortisol_saliva <- pain_data_corrected %>%
  ggplot() +
  aes(x = cortisol_saliva,
      y = pain) +
  geom_point() +
  geom_smooth()

grid.arrange(histogram_cortisol_saliva, plot_cortisol_saliva, nrow = 1)

# Explore the distribution of all the variables:

histogram_comparison <- grid.arrange(histogram_pain, histogram_sex, histogram_age, histogram_STAI, histogram_pain_cat, histogram_mindfulness, histogram_cortisol_serum, histogram_cortisol_saliva) 

# Explore the correlations between all continuous variables:

table_correlations <- pain_data_corrected %>%
  select(-ID, -sex) %>%
  drop_na() %>%
  cor()  

view(table_correlations)

# We can already suggest that the premise of "Multicolinearity" of the linear Regression 
# may be violated in mod2, since the correlation between the Cortisol levels = 0.89284798

# Model 1

# In this linear regression model we predicted the perceived pain assessed on a ten
# point scale with age and sex as predictors.

mod1 <- lm(pain ~ age + sex, data = pain_data_corrected)

mod1

# Model 2

# In the following linear regression model we predicted the perceived pain assessed on a ten
# point scale with age, sex, STAI, pain catastrophizing, mindfulness and Cortisol measures (Serum + Saliva) as predictors.

mod2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = pain_data_corrected)

mod2

# Looking at Leverage and Cook's distance

mod1 %>%
  plot(which = 5)

mod1 %>%
  plot(which = 4)

# 3 values load high on Leverage and Cook's distance. Let's look at those:

pain_data_corrected %>%
  slice(c(99, 127, 140))

mod2 %>%
  plot(which = 5)

mod2 %>%
  plot(which = 4)

# 3 values load high on Leverage and Cook's distance. Let's look at those:

pain_data_corrected %>%
  slice(c(68, 99, 113))

# All 5 values seem to be not the result of an error and therefore valid data
# Despite having a Cook's distance higher than 4/n, we will not exclude data at this point

# Assumptions
#   Normality
#   Linearity
#   Homoscedasticity
#   No Multicolinearity

# Since we already found a high correlation between the two measurements of Cortisol
# r = 0.89 we will check for Multicolinearity first:

# Checking model 2 for No Multicolinearity

mod2 %>%
  vif()

# The results of the VIF exaggerate the correlation of both Cortisol measurements
# This assumption is therefore not accepted.
# Since we know that the relation between Cortisol in the serum of the blood and 
# stress seems to be more reliable, we will remove the variable of Cortisol Saliva
# out of the model (Theory-based decision)

mod2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = pain_data_corrected)

# Checking the updated model 2 for the Multicolinearity assumption

mod2 %>%
  vif()

# Now, after excluding saliva, we can accept the Multicolinearity assumption

# Checking model 2 for Normality by QQ-PLot

mod2 %>%
  plot(which = 2)

residuals_mod2 <- enframe(residuals(mod2))
residuals_mod2 %>%
  ggplot() +
  aes(x = value) +
  geom_histogram()

describe(residuals(mod2))

# Since the QQ-plot seems to only include some minor curvatures and the skewness
# and kurtosis lie between -1 and 1, we can accept the assumption of Normality

# Checking model 2 for Linearity

mod2 %>%
  residualPlots()

# Even though there are minor curvatures visible on the plots, the tests are all
# non significant, so we accept the linearity assumption

# Checking model 2 for Homoscedasticity

mod2 %>%
  plot(which = 3)

mod2 %>%
  ncvTest()

mod2 %>%
  bptest()

# Since we cannot spot a funnel-shape in the plot and there are no significant
# test results of the NCV and the Breusch-Pagan test, we can accept the 
# assumption of Homoscedasticity

# Model 1

mod1

summary(mod1)
AIC(mod1)

# The multiple regression model was significantly better than the null model,
# explaining 5.27% of the variance in perceived pain (F(2,155) = 5.37, p < 0.01,
# Adj. R^2 = 0.05, AIC = 586.02)

# Regression equation: f(x) = 8.23 + (-0.08*x1) + (-0.04*x2)

RSS1 <- sum((pain_data_corrected$pain - predict(mod1))^2)

RSS1

mod_mean1 <- lm(pain ~ 1, data = pain_data_corrected)

TSS1 <- sum((pain_data_corrected$pain - predict(mod_mean1))^2)

TSS1


# Predictors added value to model - table:

coef_table(mod1)

# Model 2

mod2

summary(mod2)
AIC(mod2)

# The multiple regression model was significantly better than the null model,
# explaining 47.36% of the variance in perceived pain (F(6,151) = 24.54, p < 0.001,
# Adj. R^2 = 0.47, AIC = 497.06)

# Regression equation: f(x) = 1.84 + (-0.04*x1) + (0.28*x2) + (-0.02*x3) + (0.12*x4) + (-0.28*x5) + (0.56*x6)

RSS2 <- sum((pain_data_corrected$pain - predict(mod2))^2)

RSS2

mod_mean2 <- lm(pain ~ 1, data = pain_data_corrected)

TSS2 <- sum((pain_data_corrected$pain - predict(mod_mean2))^2)

TSS2

# Predictors added value to model - table:

coef_table(mod2)

# Comparison of both models

# Comparing two models via adj. R^2

summary(mod1)$adj.r.squared
summary(mod2)$adj.r.squared

# The variance explained has increased substantially by adding more information
# to the model
# Model 1: R^2 = 0.05 < Model 2: R^2 = 0.47

# Comparing two models via AIC

AIC(mod1)
AIC(mod2)

# Significant difference since 2 points difference
# AIC(mod1) = 586.02 > AIC(mod2) = 497.06

# Comparing the models based on their residual error and degrees of freedom by using an ANOVA.
# Possible since mod1 is nested within mod2

anova(mod1, mod2)

# The ANOVA supposes (similar to AIC and R^2) that mod2 is a better model than mod1