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
print("Loading preprocessed data")

load(DUMP_MEANS)
load(DUMP_CONFIDENCE)

load(DUMP_DATA)

print("Loading new measurements")
today <- read.csv(DATA_INCREMENTAL)

#x <- today[,"system_masta.lab_median_uptime"]
#print(tail(x))
#print(!is.unsorted(x))

#tail <- nrow(today) - PREDICT_BINS

print("shufflin and ignoring black swans")
## new streams may have entered out system, we cannot compare those and will ignore
ordering <- colnames(d)
newNames <- colnames(today)
today <- today[,ordering]
gc()

print("indentifying gauge vs counter for new data")
gauges <- apply(today, 2, isGauge)
counters <- apply(today, 2, isCounter)
gc()

print("getting non-negative derivative for new gauges")
today <- cbind(today[,gauges], apply(today[,counters], 2, non_negative_derivative))
gc()

#print(tail(today[,"system_masta.lab_median_uptime"]))

if (compColNames(d, today) == FALSE){
  print("comparing column names between today and yesterday, subsetting only trained series")
  today <- today[,colnames(d)]
}
sensorNames <- colnames(d)

d <- rbind(d, today)
gc()

d <- standardize(d)

if (!is.na(SMOOTH_INCRE)) {
  print("smoothing series via standard moving average")
  d <- apply(d, 2, ma1, n = SMOOTH_INCRE, sides = 1)
  # this will be a bad idea
  d[is.na(d)] <- 0
  gc()
}

print("separating training and comparison data")

d2_idx <- c( (nrow(d) - PREDICT_BINS + 1):nrow(d) )
d2 <- d[d2_idx,]
d <- d[-d2_idx,]

print("PREPROCESSING END")

# PCA
print("doing pca")
means <- colMeans(d)

print("mean centering data")
d <- t(t(d) - means)
gc()

# TODO - incremental implementation of covar matrix
print("calculating covariance matrix")
c <- cov(d)

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
tail <- matrix(,ncol=ncol(Ptrunc),nrow=(SEASONS - nrow(today) + PREDICT_BINS))
Ptrunc <- rbind(Ptrunc, tail)
gc()

print("calculating trend via weighted moving average")
trendpatterns <- apply(Ptrunc, 2, ma2, n = SEASONS, slide=TRUE)
gc()

print("decomposing trend patterns from truncated series")
seasonalCoeff <- Ptrunc - trendpatterns
gc()

print("calculating aggregate seasonal combonent by taking means of seasonal coefficients")
sI <- apply(seasonalCoeff, 2, seasonalIndex2, cycles = ( CYCLES - 1) , seasons = SEASONS)

print("adjusting truncated data by removing seasonal component")
adjusted <- matrix(,nrow = nrow(Ptrunc), ncol=ncol(Ptrunc))
for(i in seq_along(1:ncol(Ptrunc))){
  adjusted[,i] <- adjust(Ptrunc[,i], sI[,i], seasons = SEASONS, cycles = ( CYCLES + 1 ))
}

print("removing tail from trunc data")
useless <- apply(Ptrunc, 1, function(x) all(is.na(x)))
Ptrunc <- Ptrunc[!useless,]

print("taking linear regression of adjusted data to calculate future trends")
slr <- apply(adjusted, 2, autoregression)

print("opening a crystal ball to see into the future (or looking into tea leaves, whatever floats your boat)")
future_idx <- seq(nrow(Ptrunc)+1,nrow(Ptrunc)+PREDICT_BINS,1)
future <- lapply(slr, predictTrends, t=future_idx)
future <- matrix(unlist(future), nrow=PREDICT_BINS, ncol=ncol(Ptrunc))
future <- future + sI[future_idx%%SEASONS,]
gc()

print("just throwing something and hoping it sticks (read - using daily means as prediction for non-principal components)")
boring <- rotateD[,c((HIDDEN+1):ncol(rotateD))]

tail <- matrix(,ncol=ncol(boring),nrow=(SEASONS - nrow(today) + PREDICT_BINS))
boring <- rbind(boring, tail)

# function was more useful that I thought while naming it, too lazy to name it something more generic
boring <- apply(boring, 2, seasonalIndex2, cycles = ( CYCLES + 1 ), seasons = SEASONS)
predictions <- cbind(future, boring[future_idx%%SEASONS,])
boring <- NA
future <- NA
gc()

print("going back to kansas")
predictions <- t(predictions %*% t(e$vectors)) + means
predictions <- t(predictions)
colnames(predictions) <- sensorNames

print("creating outlier scores")
deviations <- predictions - d2

means <- apply(deviations, 2, mean)
devs <- apply(deviations, 2, sd)

print("normalizing delta values between original and predicted")
deviations <- ( t(deviations) - means ) / devs
deviations <- t(deviations)
gc()

print("looking for alerts in ALERT_BINS")
confident <- deviations[,which(dt > CONFIDENCE)]
confident <- abs(confident)
confident <- confident[c((nrow(confident) - ALERT_BINS):(nrow(confident) - 1) ), ]

notice <- alertBin(confident, thresh = ALARM_NOTICE)
warn <- alertBin(confident, thresh = ALARM_WARN)
crit <- alertBin(confident, thresh = ALARM_CRIT)

names <- c()
for( i in seq_along(1:nrow(confident))){
  N <- names(which(confident[i, ] > REPORT))
  if (length(N)>0) {
    names <- c(names, unlist(strsplit(N, split = "_")))
  }
}

names <- strsplit(unlist(names), split = "\\.lab")
names <- strsplit(unlist(names), split = "coe\\.")
names <- gsub("\\.", "/", unlist(names))
names <- gsub("elasticsearch", "elastic", unlist(names))
names <- names[!names %in% c("mean", "median", "n", "all", "in", "")]

names <- unlist(names)
names <- table(names)

#library(wordcloud)
#dark2 <- brewer.pal(6, "Dark2")
#wordcloud(names(names), unlist(names), max.words=200, min.freq=1, col=dark2, scale=c(4,.5), random.order = FALSE)

#tikz(file = "latex/img/cloudz.tex")
library(wordcloud)
op <- par(cex=2.0)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(names), unlist(names), max.words=200, min.freq=1, col=dark2, random.order = FALSE)
dev.off()

print(names)
#test <- lapply(notice, splitLabels)

if (DEBUG==TRUE){
  source("lib/plotSome.R")
}

names <- data.frame(names)
names$ts <- Sys.time()
#names$id <- "latest"

#if (!(is.na(ELASTIC))){
#  library("elasticsearchr")
#  es <- elastic(ELASTIC, ELASTIC_IDX, "data")
#  es %index% names
#}

#adjusted <- matrix(,nrow = nrow(Ptrunc), ncol=ncol(Ptrunc))
#x <- Ptrunc[,1]
#y <- sI[,1]
#seasons <- SEASONS
#cycles <- CYCLES + 1
#x <- matrix(x, nrow=seasons, ncol = cycles) - y
#x <- as.vector(x)

# TESTING GROUNDS

#print("fixing sums ordering")
#sums <- sums[ordering]
#sums <- sums[useful]

#sumsSquared <- sumsSquared[ordering]
#sumsSquared <- sumsSquared[useful]

#print("calculating new sums and means")
## http://rebcabin.github.io/blog/2013/01/22/covariance-matrices/
#newsums <- apply(d, 2, sum)
#sums2 <- sums + newsums
#sumsSquared2 <- sumsSquared + newsums^2
#count2 <- count + nrow(d)
#means2 <- sums2 / count2

#variance <- ( sumsSquared2 / count2 ) - means2^2
#devs2 <- sqrt(variance)

#print("normalizing data via standardization method")
#d <- ( t(d) - means2 ) / devs2
#d <- t(d)

#print("mean-centering for justice, but its useless because we already normalized to mean=0, sd=1")
#dCentered <- t(d) - colMeans(d)
#dCentered <- t(dCentered)
