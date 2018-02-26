# clear everything
rm(list=ls())

# set working directory, all library and data imports will rely on this variable
# NOTE - this should be changed to reflect your environment
setwd("/home/dev/final")

# import libs

# functions
meanCenter <- function(x){
  return(x - mean(x))
}

# load data
load("/mnt/tut-dm-final/raw.RData")

# main

cols <- colnames(d)
rows <- rownames(d)

d[is.na(d)] <- 0
#d <- t(d)
gc()
#c <- cov(d)

d <- apply(d, 2, meanCenter)
gc()
save(d, file="/mnt/tut-dm-final/meanCentered.RData")

c <- cov(d)
gc()
save(c, file="/mnt/tut-dm-final/cov.RData")

eig <- eigen(c)
c <- NA
gc()
save(eig, file="/mnt/tut-dm-final/eigen.RData")

pca <- d%*%eig$vectors
gc()
save(pca, file="/mnt/tut-dm-final/pca-rotated.RData")

cor <- cor(pca)
gc()
save(cor, file="/mnt/tut-dm-final/cor-pca.RData")

pca <- NA
cor <- NA
gc()

cor <- cor(d)
gc()
save(cor, file="/mnt/tut-dm-final/cor-raw.RData")

sd <- sd(pca)
gc()
save(sd, file="/mnt/tut-dm-final/sd-pca.RData")

sd <- NA
gc()

sd <- sd(d)
gc()
save(sd, file="/mnt/tut-dm-final/sd-raw.RData")

d <- t(d)
gc()

# range normalize
mins <- apply(d, 2, min)
gc()
maxs <- apply(d, 2, max)
gc()
diff <- maxs - mins
d <- ( d - mins) / diff
gc()
save(d, file="/mnt/tut-dm-final/range-normalized-raw.RData")

# load data
load("/mnt/tut-dm-final/raw.RData")

# standardization normalize
colSD <- apply(d, 2, sd)
gc()
d <- ( d - colMeans(d) ) / colSD
gc()
save(d, file="/mnt/tut-dm-final/standard-normalized-raw.RData")

