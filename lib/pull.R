# clear everything
rm(list=ls())

# set working directory, all library and data imports will rely on this variable
# NOTE - this should be changed to reflect your environment
setwd("/home/vagrant/final")

# import libs
library(httr)
library(jsonlite)
library(curl)

# define functions
influxQuery <- function(query) {
  return(httr::GET("http://10.0.242.104:8086/query?chunked=true", query = list(db="yellow", q=query), accept_json()))
}
influxQueryData <- function(m, from, to) {
  print(m)
  tags <- "host, node_name, path, interface"
  history <- "5d"
  bin <- "1m"
  q <- paste("http://10.0.242.104:8086/query?chunked=true&db=yellow&q=SELECT mean(*) FROM", m, "WHERE time >", from, "AND time <", to, "GROUP BY ", tags, ", time(", bin,")")
  d <- jsonlite::stream_in(curl(URLencode(q)))

  columns <- c()
  if (is.list(d[[1]][[1]]$series)) {
    rowcount <- nrow(d[[1]][[1]]$series[[1]]$values[[1]])
    print(rowcount)
    rows <- rowcount
    for ( i in seq_along(1:length(d[[1]]))){
      if (i > 1 & identical(d[[1]][[1]]$series[[1]]$tags, d[[1]][[i]]$series[[1]]$tags)) {
        rowcount <- nrow(d[[1]][[i]]$series[[1]]$values[[1]])
        rows <- rows + rowcount
      }
    }
    matr <- matrix(,ncol = 0,nrow = rows)
    timestamps <- c()
  } else {
    matr <- NA
    timestamps <- NA
  }

  for ( i in seq_along(d[[1]])) {
    # remove pointless structure
    if (is.list(d[[1]][[i]]$series)){
      data <- d[[1]][[i]]$series[[1]]
      # merge any influxdb chunks
      for ( j in i:length(d[[1]])){
        if (i != j){
          if ( identical(data$tags, d[[1]][[j]]$series[[1]]$tags)&i!=j ){
            data$values[[1]] <- rbind(data$values[[1]], d[[1]][[j]]$series[[1]]$values[[1]])
            d[[1]][[j]]$series <- NA
          }
        }
      }
      if (!is.na(d[[1]][[i]]$series)) {
        # I just modified this in previous horrible block
        rowcount <- nrow(data$values[[1]])
        timestamps <- data$values[[1]][,1]
        # we already have timestamp as rowname
        data$values[[1]] <- data$values[[1]][,-1]
        data$columns[[1]] <- data$columns[[1]][-1]
        #d[[1]][[i]] <- data
        d[[1]][[i]] <- NA
        gc()
        columns <- c(columns, paste(m, paste(data$columns[[1]], paste(data$tags[data$tags != ""], collapse = "_"), sep = "_"), sep = "_"))
        # json data is pulled as
        rows <- nrow(data$values[[1]])
        cols <- ncol(data$values[[1]])
        data$values[[1]] <- mapply(data$values[[1]], FUN=as.numeric)
        data$values[[1]] <- matrix(data=data$values[[1]], ncol=cols, nrow=rows)
        matr <- cbind(matr, data$values[[1]])
      }
      gc()
    }
  }
  colnames(matr) <- columns
  if (!is.na(timestamps[1])) {
    rownames(matr) <- timestamps
  }
  gc()
  return(matr)
}
decodeResp <- function(x){
  x <- content(x, as = "parsed", type = "application/json")
  return(x)
}
encap <- function(x){
  return(paste(c("'", x, "'"), collapse = ""))
}

# main

m <- influxQuery("SHOW MEASUREMENTS")
m <- content(m, as = "parsed", type = "application/json")
m <- unlist(m$results[[1]]$series[[1]]$values)

#m <- c("cpu")

from <- Sys.time()
format <- '%Y-%m-%d %H:%M:%S'

minutes <- 60
hours <- 24
days <- 7
period <- 60*minutes*hours*days

to <- format(Sys.time(), format = format)
from <- format(Sys.time() - period, format = format)

# influxdb needs timestamps to be encapsulated in single quotes, don't ask me why
to <- encap(to)
from <- encap(from)

d <- sapply(m, influxQueryData, from = from, to = to)
d <- do.call(cbind, d)
# first row is going to be an incomplete bin
d <- d[-1,]
d[is.na(d)] <- 0

save(d, file="/mnt/tut-dm-final/raw.RData")

colnames(d) <- NULL
rownames(d) <- NULL
save(d, file="/mnt/tut-dm-final/raw-noname.RData")
