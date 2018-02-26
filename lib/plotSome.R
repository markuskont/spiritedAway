if(PLOT_DEVIATIONS==TRUE) {
  print("plotting measured deviations for high confidence series")
  pdf("img/incre-deviations.pdf")
  for( i in seq_along(1:length(dt))){
    if (dt[i] > CONFIDENCE) {
      max <- max(deviations[,i], d2[,i], predictions[,i])
      min <- min(deviations[,i], d2[,i], predictions[,i])
      
      info <- paste(sensorNames[i], dt[i])
      plot(deviations[,i], type="l", sub=info, ylim = c(min, max))
      lines(predictions[,i], col="red")
      lines(d2[,i], col="blue")
    }
  }
  dev.off()
}