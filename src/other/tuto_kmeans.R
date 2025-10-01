library(tidyverse)
library(cluster)
library(factoextra)


# Data preparation --------------------------------------------------------

df <- USArrests

df <- na.omit(df)

df <- scale(df)
head(df)


# Clustering Distance Measures --------------------------------------------

