# Geoffrey Patching Assignment 2

# Include a short introduction

# The following packages were used:

library(MASS)
library(smacof)

# Retrieving dataset and transforming it into a matrix

Matrix_nations <- data.matrix(Nations)

# Data from:
# Kruskal, J.B., & Wish, M. (1978). Multidimensional scaling. Series Quantitative 
# Applications in the Social Sciences, NUmber II. Sage Publications.

# Wish, M., Deutsch, M., & Biener, L. (1970). Differences in the conceptual structure
# of nations: An exploratory study. Journal of Personality and Social Psychology,
# 16, 361-373.

# Use multidimensional scaling to examine the students' perceived dissimilarities 
# between the nations.
# Since that the similarity rated by the students is very subjective, we cannot use 
# the classical scaling, since for this we need metrical scaling.
# So therefore we will use non-metrical multidimensional scaling.

# Transforming the similarity matrix to a dissimilarity matrix:
# Calculating the euclidean distances

Dis_ma_nations <- sim2diss(Matrix_nations, method = 9, to.dist = TRUE)

Scaling <- isoMDS(Dis_ma_nations)

# stress = 18.85868 

plot((Scaling$points[,1]), (Scaling$points[,2]), xlab = "Coordinate 1", ylab = "Coordinate 2",
     xlim = range((Scaling$points[,1]))*1.2, type = "n")
text((Scaling$points[,1]),(Scaling$points[,2]), labels = colnames(Matrix_nations), cex = 1)

Nations_shepard <- Shepard(Dis_ma_nations, Scaling$points)
plot(Nations_shepard, pch = 20, xlab = "Dissimilarity", 
     ylab = "Distance", xlim = range(Nations_shepard$x),
     ylim = range(Nations_shepard$x))
lines(Nations_shepard$x, Nations_shepard$yf, type = "S" )

# A result section (templates from literature, but use your own words)

# A short discussion (conclusions regarding the model)