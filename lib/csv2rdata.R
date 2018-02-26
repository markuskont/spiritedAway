# clear everything
rm(list=ls())

# set working directory, all library and data imports will rely on this variable
# NOTE - this should be changed to reflect your environment
setwd(PROJECT_ROOT)

# import libs
#library(gpuR)
source("lib/util.R")

# global script params

# global script params, separated to another R file for modularity
# that way we can use same params for batch and incremental parts of this tool
source("params.R")

# load some data
d <- read.csv("/mnt/tut-dm-final/1.csv")

dataPath <- function(x){
  return(paste(c("/mnt/tut-dm-final/", x, ".csv"), collapse = ""))
}

train1 <- CYCLES - 1
train2 <- CYCLES
for ( i in 2:train1){
  path <- dataPath(i)
  print(path)
  d <- rbind(d, read.csv(path))
  gc()
}
d2 <- read.csv(dataPath(train2))

d[is.na(d)] <- 0
#colnames(d) <- c(1:ncol(d))

sensorNames <- colnames(d)

save(d, file=DUMP_TRAIN_1)
save(d2, file=DUMP_TRAIN_2)
