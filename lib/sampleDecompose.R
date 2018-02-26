
library(tikzDevice)
tikz(file = "latex/img/decompose-wrong.tex")
i <- 1
plot(Ptrunc[,i], type="l", sub=i, xlim = c(1, nrow(Ptrunc)))
lines(trendpatterns[,i], col = "red")
lines(seasonalCoeff[,i], col = "blue")
lines(rep(sI[,i], CYCLES), col = "green")
dev.off()

i <- "disk_kafka.0.lab_.srv_mean_inodes_free"

  #print("plotting actual series with PCA-restored predictions")
tikz(file = "latex/img/predict-bad.tex")
    info <- paste(sensorNames[i], dt[i])
    
    # this is very bad in R, but I just want to get PoC working
    ymin <- min(d[,i], predictions[,i], d2[,i])
    ymax <- max(d[,i], predictions[,i], d2[,i])
    
    plot(d[,i], type="l", sub=info, xlim = c(1,CYCLES*SEASONS), ylim = c(ymin, ymax))
    lines(c(rep(NA, nrow(d)), predictions[,i]), col="red")
    lines(c(rep(NA, nrow(d)), d2[,i]), col="blue")
  dev.off()
  
  tikz(file = "latex/img/dtw.tex")
  plot(dt, type="l", xlab = "series", ylab = "confidence")
  dev.off()
  
    #print("plotting predicted period with detected anomalies")
    #pdf("img/anomalies.pdf")
  tikz(file = "latex/img/out-bad.tex")
    i <- "net_snake.gw.lab_eth1_mean_packets_recv"
    j <- 1
        info <- paste(sensorNames[i], dt[i])
        
        # this is very bad in R, but I just want to get PoC working
        ymin <- min(d[,i], predictions[,i], d2[,i])
        ymax <- max(d[,i], predictions[,i], d2[,i])
        
        plot(d[,i], type="l", sub=info, xlim = c(1,CYCLES*SEASONS), ylim = c(ymin, ymax))
        lines(c(rep(NA, nrow(d)), d2[,i]))
        
        #plot(d[,i], sub=info, type="l" )
        
        green <- rep(NA, length(d2[,i]))
        green[noticePerSeries$net_snake.gw.lab_eth1_mean_packets_recv] <- d2[noticePerSeries$net_snake.gw.lab_eth1_mean_packets_recv, i]
        green <- c(rep(NA, nrow(d)), green)
        points(green, col = "green")
        
        yellow <- rep(NA, length(d2[,i]))
        yellow[warnPerSeries$net_snake.gw.lab_eth1_mean_packets_recv] <- d2[warnPerSeries$net_snake.gw.lab_eth1_mean_packets_recv, i]
        yellow <- c(rep(NA, nrow(d)), yellow)
        points(yellow, col = "yellow")
        
        red <- rep(NA, length(d2[,i]))
        red[critPerSeries$net_snake.gw.lab_eth1_mean_packets_recv] <- d2[critPerSeries$net_snake.gw.lab_eth1_mean_packets_recv, i]
        red <- c(rep(NA, nrow(d)), red)
        points(red, col = "red")
        
        #lines(deviations[,i], col = "blue")
    dev.off()
    
    tikz(file = "latex/img/cloudz.tex")
    library(wordcloud)
    op <- par(cex=2.0)
    dark2 <- brewer.pal(6, "Dark2")
    wordcloud(names(names), unlist(names), max.words=200, min.freq=1, col=dark2, random.order = FALSE)
    dev.off()