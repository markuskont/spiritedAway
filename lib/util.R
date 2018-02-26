# functions
dataPath <- function(x){
  return(paste(c("/mnt/tut-dm-final/", x, ".csv"), collapse = ""))
}

meanCenter <- function(x){
  return(x - mean(x))
}

standardize <- function(x){
  colSD <- apply(x, 2, sd)
  gc()
  return(( x - colMeans(x) ) / colSD)
}

# gauges can increase and decrease at will
isGauge <- function(x){
  return(is.unsorted(x))
}

# counters can only increase in value
isCounter <- function(x){
  return(!is.unsorted(x))
}

# handle counters the influx way
derivative <- function(x){
  x <- c(x[-1] - x[-length(x)])
  return(c(x, x[length(x)]))
}

non_negative_derivative <- function(x) {
  return(abs(derivative(x)))
}

ma1 <- function(x, n, sides = 1){
  return(filter(x, rep(1/n,n), sides = sides))
}

ma2 <- function(x, n = 60, slide = TRUE){
  # we do weigted moving average, as opposed to 2-ma of N-ma
  if (slide == TRUE){
    partial <- 1/(n*2)
    filter <- c(partial, rep(1/n, n-1), partial)
  } else {
    filter <- c(rep(1/n, n))
  }
  return(filter(x, filter = filter, sides=2))
}

normalize <- function(x, stdevs, method="standard") {
  x <- t(x)
  if (method == "standard") {
    return(t(( x - colMeans(x) ) / apply(x, 1, sd)))
  } else if (method == "range") {
    return(x)
  }
}

seasonalIndex <- function(x, cycles, seasons){
  x <- matrix(x, nrow = SEASONS, ncol = cycles)
  x[,1] <- ifelse(is.na(x[,1]), x[,ncol(x)], x[,1])
  x <- x[,-ncol(x)]
  # get seasonal index
  x <- rowMeans(x)
  return(x)
}

# incremental script does not use data that is split evenly between days
# thus, we should fill NA values in last column from the first, not vice-versa
# otherwise we will lose information from today
seasonalIndex2 <- function(x, cycles, seasons){
  x <- matrix(x, nrow = seasons, ncol = cycles)
  
  last <- x[,ncol(x)]
  
  x[,ncol(x)] <- ifelse(is.na(x[,ncol(x)]), x[,1], x[,ncol(x)])
  
  x <- x[,-1]
  # get seasonal index
  x <- rowMeans(x)
  return(x)
}

trend <- function(x, intercept, t){
  return(t*x+intercept)
}

predictTrends <- function(lr,t = c(1,2,3)){
  return(t*lr$coefficients[2]+lr$coefficients[1])
}

adjust <- function(x,y, cycles, seasons){
  x <- matrix(x, nrow=seasons, ncol = cycles) - y
  return(as.vector(x))
}

autoregression <- function(x){
  return(lm(as.vector(x)~c(1:length(x))))
}

# use mean values accross cycles as predictions for less important components
meanUninteresting <- function(x, cycles, seasons){
  x <- matrix(x, nrow = seasons, ncol = cycles)
  x <- rowMeans(x)
  return(x)
}

meanUninteresting2 <- function(x, cycles, seasons){
  x <- matrix(x, nrow = seasons, ncol = cycles)
  x <- rowMeans(x)
  return(x)
}

slowDoDTW <- function(x,y){
  dt <- c(rep(NA, ncol(x)))
  for( i in seq_along(1:ncol(x))){
    dt[i] <- dtw(x[,i], y[,i])$distance
  }
  return(dt)
}

fasterButNotSoMuchDoDTW <- function(x,y){
  return(sapply(1:ncol(x), function(i) dtw(x[,i], y[,i])$distance))
}

alertCountPerConfidence <- function(x, y){
  return(length(which(x > y)))
}

alertLevel <- function(x, thresh = 3){
  return(which(x > thresh))
}

COVexample <- function(x,y) {
  x.bar <- mean(x)
  y.bar <- mean(y)
  N <- length(x)
  
  Cov <- (sum((x-x.bar)*(y-y.bar))) / (N-1)
  return(Cov)
}

checkIn <- function(x, y){
  return(x[!(x %in% y)])
}

compColNames <- function(x, y){
  return(identical(colnames(x), colnames(y)))
}

standardize <- function(x){
  print("normalizing data via standardization method")
  devs <- apply(x, 2, sd)
  gc()
  x <- ( t(x) - colMeans(x) ) / devs
  x <- t(x)
  gc()
  return(x)
}

alertBin <- function(x, thresh = 3){
  x <- apply(x, 2, alertLevel, thresh = thresh)
  x <- lapply(x, length)
  x <- unlist(x)
  x <- x[x > 0]
  return(x)
}