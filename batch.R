# clear everything
rm(list=ls())

# set working directory, all library and data imports will rely on this variable
# NOTE - this should be changed to reflect your environment
setwd(PROJECT_ROOT)

# import libs
#library(gpuR)
source("lib/util.R")

# global script params, separated to another R file for modularity
# that way we can use same params for batch and incremental parts of this tool
source("params.R")

# load some data
print("loading data from CSV files")
train1 <- CYCLES - 1
train2 <- CYCLES
d <- read.csv(dataPath(1))
for ( i in 2:train1){
  path <- dataPath(i)
  print(path)
  d <- rbind(d, read.csv(path))
  gc()
}
d2 <- read.csv(dataPath(train2))

print("PREPROCESSING START")
print("binding forecasting and comparison data")
d <- rbind(d, d2)

d[is.na(d)] <- 0

print("indentifying gauge vs counter")
# identify counter vs gauge before normalization
gauges <- apply(d, 2, isGauge)
counters <- apply(d, 2, isCounter)

gc()

print("getting non-negative derivative for gauges")
d <- cbind(d[,gauges], apply(d[,counters], 2, non_negative_derivative))
gc()

#print("rounding data points to reasonable ranges, useful if you want to use GPU libraries")
#d <- round(d, digits = 2)
#gc()

print("calculating standard deviations")
devs <- apply(d, 2, sd)

gc()

print("fetching col prior to pruning")
sensorNamesOrig <- colnames(d)

print("subsetting non-zero series")
# only consider columns that do not flatline, but store indices just in case
useful <- which(devs!=0)
d <- d[,useful]
gc()

print("fetching col names and saving to disk")
sensorNames <- colnames(d)
save(sensorNames, file=DUMP_NAMES)

print("calculating column means and item counts, and storing to disk")
sums <- apply(d, 2, sum)
sumsSquared <- apply(d^2, 2, sum)
means <- colMeans(d)
count <- nrow(d)

print("saving calculations to disk")
save(sums, file=DUMP_SUMS)
save(sumsSquared, file=DUMP_SUMSQUARE)
save(means, file=DUMP_MEANS)
save(count, file=DUMP_COUNTS)

print("saving unnormalized data to disk")
save(d, file=DUMP_DATA)

gc()

#print("normalizing data via range method")
#mins <- apply(d,2,min)
#maxs <- apply(d,2,max)
#d <- ( t(d) - mins ) / ( maxs - mins )
#d <- t(d)
#gc()

#print("normalizing data via standardization method")
#d <- ( t(d) - colMeans(d) ) / devs[useful]
#d <- t(d)
d <- standardize(d)

gc()

if (!is.na(SMOOTH_BATCH)) {
  print("smoothing series via standard moving average")
  d <- apply(d, 2, ma1, n = SMOOTH_BATCH, sides = 1)
  # this will be a bad idea
  d[is.na(d)] <- 0
  gc()
}

print("separating training and validation data")
d2_idx <- c(( (nrow(d)+1)-SEASONS ):nrow(d))
d2 <- d[d2_idx,]

colnames(d2) <- sensorNames
d <- d[-d2_idx,]

colnames(d) <- sensorNames
gc()

print("PREPROCESSING END")

# PCA
print("doing pca")
means <- colMeans(d)

print("mean centering data")
d <- t(t(d) - means)
gc()

print("calculating covariance matrix")
c <- cov(d)
print("saving covariance matrix to disk")
save(c, file=DUMP_COVAR)

print("calculating eigenvectors")
e <- eigen(c)

if (is.na(HIDDEN)) {
  print("estimating hidden values")
  HIDDEN <- length(which(e$values>1))
}

print("rotating to new space")
rotateD <- d%*%e$vectors

print("truncating rotated matrix to get top hidden vectors")
Ptrunc <- rotateD[,c(1:HIDDEN)]
gc()

print("calculating trend via weighted moving average")
trendpatterns <- apply(Ptrunc, 2, ma2, n = SEASONS, slide=TRUE)
gc()

print("decomposing trend patterns from truncated series")
seasonalCoeff <- Ptrunc - trendpatterns
gc()

print("calculating aggregate seasonal combonent by taking means of seasonal coefficients")
sI <- apply(seasonalCoeff, 2, seasonalIndex, cycles = ( CYCLES - 1) , seasons = SEASONS)

print("adjusting truncated data by removing seasonal component")
adjusted <- matrix(,nrow = nrow(Ptrunc), ncol=ncol(Ptrunc))
for(i in seq_along(1:ncol(Ptrunc))){
  adjusted[,i] <- adjust(Ptrunc[,i], sI[,i], seasons = SEASONS, cycles = ( CYCLES - 1))
}

print("taking linear regression of adjusted data to calculate future trends")
slr <- apply(adjusted, 2, autoregression)

print("opening a crystal ball to see into the future (or looking into tea leaves, whatever floats your boat)")
future <- lapply(slr, predictTrends, t=seq(nrow(Ptrunc)+1,nrow(Ptrunc)+SEASONS,1))
future <- matrix(unlist(future),nrow=SEASONS, ncol=ncol(sI))
future <- future + sI
gc()

print("just throwing something and hoping it sticks (read - using daily means as prediction for non-principal components)")
boring <- rotateD[,c((HIDDEN+1):ncol(rotateD))]
boring <- apply(boring, 2, meanUninteresting, cycles = ( CYCLES - 1 ), seasons = SEASONS)

predictions <- cbind(future, boring)

print("going back to kansas")
predictions <- t(predictions %*% t(e$vectors)) + means
predictions <- t(predictions)
colnames(predictions) <- sensorNames

print("performing DTW to measure overall accuracy of our approach")
library(dtw)
library(parallel)
detectCores()
dt <- mclapply(1:ncol(predictions), function(x) dtw(predictions[,x], d2[,x])$distance)
dt <- unlist(dt)

print("normalizing DTW distances via range method")
dt <- ( dt -  min(dt)) / ( max(dt) - min(dt) )
dt <- 1 - dt
plot(dt, type="l", xlab = "series", ylab = "confidence")
print("saving confidence levels")
names(dt) <- sensorNames
save(dt, file=DUMP_CONFIDENCE)

print("creating outlier scores")
deviations <- predictions - d2

means <- apply(deviations, 2, mean)
devs <- apply(deviations, 2, sd)

print("normalizing delta values between original and predicted")
deviations <- ( t(deviations) - means ) / devs
deviations <- t(deviations)
gc()

confident <- deviations[,which(dt > CONFIDENCE)]
confident <- abs(confident)

print("looking for alerts over different series")
noticePerSeries <- apply(confident, 2, alertLevel, thresh = ALARM_NOTICE)
warnPerSeries <- apply(confident, 2, alertLevel, thresh = ALARM_WARN)
critPerSeries <- apply(confident, 2, alertLevel, thresh = ALARM_CRIT)

print("looking for ensemble alerts")
ensemble <- apply(confident, 1, max)

noticePerInterval <- alertLevel(ensemble, thresh = ALARM_NOTICE)
warnPerInterval <- alertLevel(ensemble, thresh = ALARM_WARN)
critPerInterval <- alertLevel(ensemble, thresh = ALARM_CRIT)

ensemble.level <- rep(0, nrow(confident))
ensemble.level[noticePerInterval] <- 1
ensemble.level[warnPerInterval] <- 2
ensemble.level[critPerInterval] <- 3

ensemble.human <- rep("OK", nrow(confident))
ensemble.human[noticePerInterval] <- "NOTICE"
ensemble.human[warnPerInterval] <- "WARN"
ensemble.human[critPerInterval] <- "CRIT"

if (SHOWTIME==TRUE){
  source("lib/lasVegas.R")
}

if (DEBUG==TRUE){
  source("lib/plotAll.R")  
}
