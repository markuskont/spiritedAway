# clear everything
rm(list=ls())

COV <- function(x,y) {
  x.bar <- mean(x)
  y.bar <- mean(y)
  N <- length(x)
  
  #Cov <- (sum((x-x.bar)*(y-y.bar))) / (N-1)
  
  Cov <- (sum(x*y) / N-1) - sum(x)*( sum(y)/ (N-1)^2 )
  
  return(Cov)
}


###############
a <- c(1,2,3,4,5,6,7)
b <- c(11,12,13,14,15,16,17)
c <- c(-1,-4,-6,-1,5,6,9)
d <- c(-11,-4,-6,-111,5,6,9)

m <- cbind(a, b, c, d)

###############

c <- cov(m)

###############

#means <- colMeans(m)
sums <- apply(m, 2, sum)
N <- nrow(m)

means <- sums / N

prods <- t(m) %*% m

residual <- t( t(m) - means )
C <- t(residual) %*% residual

#m2 <- matrix(0, nrow=nrow(C), ncol=ncol(C))
#for( i in 1:ncol(m)){
#  for( j in 1:ncol(m)){
#    # m2[i, j] <- ( sum(m[,i] * m[,j])
#    m2[i, j] <- ( sum(m[,i] * m[,j]) / (N-1) ) - ( ( sums[i] / ( N-1 ) ) * (sums[j] / ( N-1 )) ) 
#  }
#}

#C2 <- ( t(m) %*% m ) - (t(means) %*% means)

C <- C / (N-1)

###############

test <- all(C == c)
print(test)

###############

item <- c(1,2,3,4)

test <- item

m2 <- rbind(m, item)
N2 <- N + 1
sums2 <- sums + item
means2 <- sums2 / N2

residual2 <- t( t(m2) - means2 )
C2 <- t(residual2) %*% residual2
C2 <- C2 / (N2 - 1)

