if(PLOT_ROTATE==TRUE) {
  print("plotting truncated hidden variables and trend patterns")
  pdf("img/hidden-trunc.pdf")
  for( i in seq_along(1:ncol(Ptrunc))){
    plot(Ptrunc[,i], type="l", sub=i, xlim = c(1, nrow(Ptrunc)))
    lines(trendpatterns[,i], col = "red")
  }
  dev.off()
}

if(PLOT_DECOMPOSE==TRUE) {
  print("plotting truncated hidden variables and trend patterns")
  pdf("img/hidden-trunc.pdf")
  for( i in seq_along(1:ncol(Ptrunc))){
    plot(Ptrunc[,i], type="l", sub=i, xlim = c(1, nrow(Ptrunc)))
    lines(trendpatterns[,i], col = "red")
    lines(seasonalCoeff[,i], col = "blue")
    lines(rep(sI[,i], CYCLES), col = "green")
  }
  dev.off()
}

if(PLOT_PREDICT_HIDDEN==TRUE) {
  print("plotting truncated hidden variables and predictions")
  pdf("img/hidden-pred.pdf")
  for( i in seq_along(1:ncol(Ptrunc))){
    plot(Ptrunc[,i], type="l", xlim = c(1,CYCLES*SEASONS))
    lines(c(rep(NA, nrow(Ptrunc)), future[,i]), col="red")
  }
  dev.off()
}

if(PLOT_PREDICT_ACTUAL==TRUE) {
  print("plotting actual series with PCA-restored predictions")
  pdf("img/actual-pred.pdf")
  for( i in seq_along(1:ncol(d))){
    info <- paste(sensorNames[i], dt[i])
    
    # this is very bad in R, but I just want to get PoC working
    ymin <- min(d[,i], predictions[,i], d2[,i])
    ymax <- max(d[,i], predictions[,i], d2[,i])
    
    plot(d[,i], type="l", sub=info, xlim = c(1,CYCLES*SEASONS), ylim = c(ymin, ymax))
    lines(c(rep(NA, nrow(d)), predictions[,i]), col="red")
    lines(c(rep(NA, nrow(d)), d2[,i]), col="blue")
  }
  dev.off()
}

if(PLOT_ANOMALIES==TRUE) {
  print("plotting predicted period with detected anomalies")
  pdf("img/anomalies.pdf")
  j <- 0
  for( i in seq_along(1:ncol(d2))){
    if (dt[i] > CONFIDENCE) {
      j <- j + 1
      info <- paste(sensorNames[i], dt[i])
      
      # this is very bad in R, but I just want to get PoC working
      ymin <- min(d[,i], predictions[,i], d2[,i])
      ymax <- max(d[,i], predictions[,i], d2[,i])
      
      plot(d[,i], type="l", sub=info, xlim = c(1,CYCLES*SEASONS), ylim = c(ymin, ymax))
      lines(c(rep(NA, nrow(d)), d2[,i]))
      
      #plot(d[,i], sub=info, type="l" )
      
      green <- rep(NA, length(d2[,i]))
      green[noticePerSeries[[j]]] <- d2[noticePerSeries[[j]], i]
      green <- c(rep(NA, nrow(d)), green)
      points(green, col = "green")
      
      yellow <- rep(NA, length(d2[,i]))
      yellow[warnPerSeries[[j]]] <- d2[warnPerSeries[[j]], i]
      yellow <- c(rep(NA, nrow(d)), yellow)
      points(yellow, col = "yellow")
      
      red <- rep(NA, length(d2[,i]))
      red[critPerSeries[[j]]] <- d2[critPerSeries[[j]], i]
      red <- c(rep(NA, nrow(d)), red)
      points(red, col = "red")
      
      #lines(deviations[,i], col = "blue")
    }
  }
  dev.off()
}

if(PLOT_DEVIATIONS==TRUE) {
  print("plotting measured deviations for high confidence series")
  pdf("img/deviations.pdf")
  for( i in seq_along(1:ncol(d))){
    if (dt[i] > CONFIDENCE) {
      info <- paste(sensorNames[i], dt[i])
      #min <- min(c(min(deviations, predictions)))
      #max <- max(c(max(deviations, predictions)))
      plot(deviations[,i], type="l", sub=info)
    }
  }
  dev.off()
}