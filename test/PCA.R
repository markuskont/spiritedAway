# clear everything
rm(list=ls())

# set working directory, all library and data imports will rely on this variable
# NOTE - this should be changed to reflect your environment
setwd("/home/vagrant/final")

# import libs
#library(gpuR)
source("lib/util.R")

PLOT <- FALSE

# load data
load("/mnt/tut-dm-final/raw-noname.RData")

d[is.na(d)] <- 0
colnames(d) <- c(1:ncol(d))

#d <- gpuMatrix(d)
#gc()

# identify counter vs gauge before normalization
gauges <- apply(d, 2, isGauge)
counters <- apply(d, 2, isCounter)

fixdCounters <- apply(d[,counters], 2, non_negative_derivative)
d <- cbind(d[,gauges], fixdCounters)
gc()
#d <- round (d, digits = 6)
gc()

devs <- apply(d, 2, sd)

gc()

# only consider columns that do not flatline, but store indices for later usage
useful <- which(devs!=0)
d <- d[,useful]
gc()

d <- ( t(d) - colMeans(d) ) / devs[useful]
d <- t(d)
gc()

reduced <- d[c(1:60), c(1:10)]
means <- colMeans(reduced)

centered <- t(t(reduced) - means)
c <- cov(centered)
e <- eigen(c)

#featureVect <- e$vectors

rotated <- centered%*%e$vectors
colnames(rotated) <- colnames(reduced)

fuckit <- data.frame()
fuckit.pca <- prcomp(reduced)
fuckit.rotated <- fuckit.pca$x
fuckit.back <- t(fuckit.rotated %*% t(fuckit.pca$rotation)) + fuckit.pca$center
fuckit.back <- t(fuckit.back)

orig <- t(rotated %*% t(e$vectors)) + means
orig <- t(orig)
