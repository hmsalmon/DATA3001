plot(filtRight$WORLDPOSX, filtRight$WORLDPOSY, cex = 0.05, xlim=c(107,130), 
     asp=1, ylim=c(450,465), type = 'l', xlab=NA, ylab=NA)
lines(filtLeft$WORLDPOSX, filtLeft$WORLDPOSY, cex = 0.05)
lines(x = x_start,y=y_start,col = "red", lwd=1)
for (i in 1:497) {
  lap = findLap(i)
  points(head(lap, 1)$WORLDPOSX, head(lap, 1)$WORLDPOSY, col='green', cex=0.3)
}

title(main='Start Line Plot', xlab = 'WORLDPOSX', ylab = 'WORLDPOSY')


plot(filtRight$WORLDPOSX, filtRight$WORLDPOSY, cex = 0.05, xlim=c(630,645), 
    asp=1, ylim=c(-218,-228), type = 'l', xlab=NA, ylab=NA)
lines(filtLeft$WORLDPOSX, filtLeft$WORLDPOSY, cex = 0.05)
lines(x = x_finish,y=y_finish,col = "red", lwd=1)
for (i in 1:497) {
  lap = findLap(i)
  points(tail(lap, 1)$WORLDPOSX, tail(lap, 1)$WORLDPOSY, col='green', cex=0.3)
}

title(main='Finish Line Plot', xlab = 'WORLDPOSX', ylab = 'WORLDPOSY')




