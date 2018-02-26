# clear everything
rm(list=ls())

# set working directory, all library and data imports will rely on this variable
# NOTE - this should be changed to reflect your environment
setwd("/home/vagrant/final")

# import libs
#library(gpuR)
source("lib/util.R")

PLOT <- TRUE
YELLOW <- TRUE
HIDDEN <- 30

# load data
if (YELLOW == TRUE) {
  load("/mnt/tut-dm-final/raw-noname.RData")
} else {
  root <- "/mnt/tut-dm-final/"
  files <- c("cpu", "mem", "system", "net", "netstat", "disk", "diskio", "kernel", "processes", "swap", "elasticsearch_breakers", "elasticsearch_cluster_health", "elasticsearch_clusterstats_indices", "elasticsearch_clusterstats_nodes", "elasticsearch_fs", "elasticsearch_http", "elasticsearch_indices", "elasticsearch_jvm", "elasticsearch_os", "elasticsearch_process", "elasticsearch_thread_pool", "elasticsearch_transport")
  
  filename <- paste(c(root, files[1], ".csv"), collapse = "")
  d <- read.csv(paste(filename, collapse = ""))
  
  for (i in seq_along(2:length(files))) {
    filename <- paste(c(root, files[i], ".csv"), collapse = "")
    #print(filename)
    d <- cbind(d, read.csv(file = filename))
    gc()
  }
  to <- nrow(d)
  from <- to - 120
  d <- d[-1,]
  #d <- d[-c(from, to),]
  gc()
}

#stop()

d[is.na(d)] <- 0
colnames(d) <- c(1:ncol(d))

sensorNames <- colnames(d)

# identify counter vs gauge before normalization
gauges <- apply(d, 2, isGauge)
counters <- apply(d, 2, isCounter)

d <- cbind(d[,gauges], apply(d[,counters], 2, non_negative_derivative))
gc()
d <- round (d, digits = 6)
gc()

devs <- apply(d, 2, sd)

gc()

# only consider columns that do not flatline, but store indices for later usage
useful <- which(devs!=0)
d <- d[,useful]
gc()

#if(PLOT==TRUE) {
#  pdf("img/raw.pdf")
#  for( i in seq_along(1:ncol(d))){
#    plot(d[,i], type="l", sub=sensorNames[i], xlim = c(1, nrow(d)))
#  }
#  dev.off()
#}

# normalize and transpose back for uniformity in code
d <- ( t(d) - colMeans(d) ) / devs[useful]
d <- t(d)
gc()

#if(PLOT==TRUE) {
#  pdf("img/normalized.pdf")
#  for( i in seq_along(1:ncol(d))){
#    plot(d[,i], type="l", sub=sensorNames[i], xlim = c(1, nrow(d)))
#  }
#  dev.off()
#}

# PCA
# mean center
means <- colMeans(d)

d <- t(t(d) - means)
gc()

c <- cov(d)
e <- eigen(c)

rotateD <- d%*%e$vectors

#if(PLOT==TRUE) {
#  pdf("img/hidden.pdf")
#  for( i in seq_along(1:ncol(d))){
#    plot(rotateD[,i], type="l", sub=i, xlim = c(1, nrow(rotateD)))
#  }
#  dev.off()
#}

Ptrunc <- rotateD[,c(1:HIDDEN)]

seasons <- 60*24
cycles <- nrow(d) / seasons

#seasons <- cycle*7

#trendpattern = filter(beerprod, filter = c(1/8, 1/4, 1/4, 1/4, 1/8), sides=2)
ma <- function(x, n = 60){
  # we do weigted moving average, as opposed to 2-ma of N-ma
  partial <- 1/(n*2)
  filter <- c(partial, rep(1/n, n-1), partial)
  return(filter(x, filter = filter, sides=2))
}

trendpatterns <- apply(Ptrunc, 2, ma, n = seasons)
seasonalCoeff <- Ptrunc / trendpatterns

if(PLOT==TRUE) {
  pdf("img/hidden-trunc.pdf")
  for( i in seq_along(1:ncol(Ptrunc))){
    plot(Ptrunc[,i], type="l", sub=i, xlim = c(1, nrow(Ptrunc)))
    lines(trendpatterns[,i], col = "red")
  }
  dev.off()
}