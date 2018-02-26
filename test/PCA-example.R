# clear everything
rm(list=ls())

# set working directory, all library and data imports will rely on this variable
# NOTE - this should be changed to reflect your environment
setwd("/home/vagrant/final")

# import libs
#library(gpuR)
#source("lib/util.R")

x <- c(2.5, 0.5, 2.2, 1.9, 3.1, 2.3, 2, 1, 1.5, 1.1)
y <- c(2.4, 0.7, 2.9, 2.2, 3.0, 2.7, 1.6, 1.1, 1.6, 0.9)

data <- cbind(x,y)
means <- colMeans(data)

meanCent <- t(data) - means
meanCent <- t(meanCent)

c <- cov(meanCent)
e <- eigen(c)

pc1 <- meanCent[,1] * e$vectors[1,1] + meanCent[,2] * e$vectors[2,1]
pc2 <- meanCent[,1] * e$vectors[1,2] + meanCent[,2] * e$vectors[2,2]
PC <- data.frame(PC1 = pc1, PC2 = pc2)
PC <- cbind(pc1, pc2)

PC_real <- meanCent%*%e$vectors

d <- data.frame()
d.pca <- prcomp(data)

plot(data, pch = 19)