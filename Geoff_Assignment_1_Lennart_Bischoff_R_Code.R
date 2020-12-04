# Geoffrey Patching - Assignment 1 - PSYP14 (HT2020)
# The Covariance Matrix / Multidimensional Scaling 

# Computing the variance-covariance and correlation matrices. 
# Aim: increase your confidence in working with data matrices. 
# Provides insight into how R works. 

# The following packages were used:

library(tidyverse)
library(psych)

# The following custom functions were used:

# [X]

view(womenshealth)

womenshealth <- womenshealth %>%
  drop_na()

Matrix_womenshealth <- data.matrix(womenshealth, rownames.force = NA)

# R function to calculate a (Pearson) covariance matrix for the 
# womenshealth.txtdata set.
# Ideally, your function will accept any number of variables 
# ( > 2, no need to test for missing values) and return a covariance 
# matrix for the variables. 

#number of variables

c <- ncol(Matrix_womenshealth)

#number of subjects

n <- nrow(Matrix_womenshealth) 

# Calculating a matrix displaying the means of the columns

Mean_womenshealth <- matrix(data=1, nrow = n) %*% colMeans(Matrix_womenshealth)

view(Mean_womenshealth)

# Creating a difference matrix via subtracting the expected value (mean) from the actual value

Difference_womenshealth <- Matrix_womenshealth - Mean_womenshealth

#creates the covariance matrix

Covariance_womenshealth <- ((n-1)^-1 * t(Difference_womenshealth)) %*% (Difference_womenshealth)

view(Covariance_womenshealth)

# Out of this knowledge we will try to build a function for the covariance matrix:

MatCov <- function(x) {
  Matrix <- data.matrix(x, rownames.force = NA)
  Mean <- matrix(data=1, nrow = nrow(Matrix)) %*% colMeans(Matrix)
  Difference <- Matrix - Mean
  Covariance <- (1/(nrow(Matrix)-1))*crossprod(Difference)
}

# Write another short function to scale the covariance matrix into 
# correlation 
# (see Everitt & Hothorn, pp. 12-14, for details)

# Constructing the correlation matrix out of the covariance matrix

Variance_Vector <- diag(Covariance_womenshealth)^(-1/2)

Correlation_womenshealth <- diag(Variance_Vector) %*% Covariance_womenshealth %*% diag(Variance_Vector)

view(Correlation_womenshealth)

# Out of this knowledge we will try to build a function for the correlation matrix:
# Note that this function always needs a Covariance function to base upon

MatCor <- function(x) {
  Diagonal <- diag(x)^(-1/2)
  Correlation <- diag(Diagonal) %*% x %*% diag(Diagonal)
}

# Check your calculations using the base R functions: 
# cov() 
# cor() 

# Comparing covariance

cov_womenshealth <- womenshealth %>%
  cov(method = "pearson")

CovWom <- MatCov(womenshealth)

view(CovWom)
view(cov_womenshealth)
view(Covariance_womenshealth)

# By comparing the three "different" ways to compute the Covariance matrix,
# we can easily see that all results are the exact same.

# Comparing correlation

cor_womenshealth <- womenshealth %>%
  cor(method = "pearson")

CorrWom <- MatCor(CovWom)

view(CorrWom)
view(cor_womenshealth)
view(Correlation_womenshealth)

# By comparing the three "different" ways to compute the Correlation matrix,
# we can easily see that all results are the exact same.