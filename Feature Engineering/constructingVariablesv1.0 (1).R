getwd()
library(dplyr) #INSTALL THIS PACKAGE IF YOU HAVENT GOT IT ALREADY

#LOAD IN FILES (SET WORKING DIRECTORY TO FOLDER WITH THESE FILES IN IT)
# turns = read.csv("f1sim-ref-turns.csv")
filtRight = read.csv("filtered_right.csv")
filtLeft = read.csv("filtered_left.csv")
#track = read.csv("f1sim-ref-line.csv")
filtCars = read.csv("filtered_cars_2023.csv")

#TABLE WITH LIST OF ALL LAPS WITH SECTOR TIME IN "CURRENT_LAP_TIME_MS" COLUMN
lapList = 
  
  filtCars %>%
  group_by(SESSION_IDENTIFIER, LAP_NUM) %>%
  slice(which.max(CURRENT_LAP_TIME_MS))

lapList = arrange(lapList, CURRENT_LAP_TIME_MS)

lapList = subset(lapList, select = c(SESSION_IDENTIFIER, LAP_NUM, CURRENT_LAP_TIME_MS, NORMALISED_LAP_TIME_MS))

#TEST HISTOGRAM
hist(lapList$CURRENT_LAP_TIME_MS, breaks = 100)

#EXTRACT DATA FOR LAP WITH THE NTH FASTEST TIME FOR RANGE
findLap <- function(rank) {
  
  sel_sessionid = lapList$SESSION_IDENTIFIER[rank]
  sel_lapnumid = lapList$LAP_NUM[rank]
  
  return(filtCars[filtCars$SESSION_IDENTIFIER == sel_sessionid & filtCars$LAP_NUM == sel_lapnumid,])
  
}

lap1 = findLap(1)
lap2 = findLap(2)
lap3 = findLap(3)
lap5 = findLap(5)
lap10 = findLap(10)
lap21 = findLap(21)
lap100 = findLap(100)
lap200 = findLap(200)
lap250 = findLap(250)
lap450 = findLap(450)
lap516 = findLap(516)
lap517 = findLap(517)

maxBrake = c()
maxBrake_S1 = c()
maxBrake_S2 = c()
maxBrake_S3 = c()
aveBrake = c()
aveBrake_S1 = c()
aveBrake_S2 = c()
aveBrake_S3 = c()

minThrottle = c()
minThrottle_S1 = c()
minThrottle_S2 = c()
minThrottle_S3 = c()
aveThrottle = c()
aveThrottle_S1 = c()
aveThrottle_S2 = c()
aveThrottle_S3 = c()

timeDecreasingThrottle = c()
timeDecreasingThrottle_S1 = c()
timeDecreasingThrottle_S2 = c()
timeDecreasingThrottle_S3 = c()
timeIncreasingThrottle = c()
timeIncreasingThrottle_S1 = c()
timeIncreasingThrottle_S2 = c()
timeIncreasingThrottle_S3 = c()
timeSameThrottle = c()
timeSameThrottle_S1 = c()
timeSameThrottle_S2 = c()
timeSameThrottle_S3 = c()

firstBrakeDist = c()
firstBrakeLateral = c()
firstBrakeSpeed = c()

#Sector 1 = 0-20% of track
#Sector 2 = 20-60% of track
#Sector 3 = 60-100% of track

cut_s12 = 0.2
cut_s23 = 0.6

for (i in 1:length(lapList$SESSION_IDENTIFIER)) {
  
  selectedLap = findLap(i)
  
  maxBrake =    c(maxBrake, max(selectedLap$BRAKE))
  maxBrake_S1 = c(maxBrake_S1, max(selectedLap$BRAKE[which(selectedLap$LAP_DISTANCE_PERC < cut_s12)]))
  maxBrake_S2 = c(maxBrake_S2, max(selectedLap$BRAKE[which((selectedLap$LAP_DISTANCE_PERC >= cut_s12) &
                                                             (selectedLap$LAP_DISTANCE_PERC <= cut_s23))]))
  maxBrake_S3 = c(maxBrake_S3, max(selectedLap$BRAKE[which(selectedLap$LAP_DISTANCE_PERC > cut_s23)]))
  
  aveBrake =    c(aveBrake, mean(selectedLap$BRAKE))
  aveBrake_S1 = c(aveBrake_S1, mean(selectedLap$BRAKE[which(selectedLap$LAP_DISTANCE_PERC < cut_s12)]))
  aveBrake_S2 = c(aveBrake_S2, mean(selectedLap$BRAKE[which((selectedLap$LAP_DISTANCE_PERC >= cut_s12) &
                                                              (selectedLap$LAP_DISTANCE_PERC <= cut_s23))]))
  aveBrake_S3 = c(aveBrake_S3, mean(selectedLap$BRAKE[which(selectedLap$LAP_DISTANCE_PERC > cut_s23)]))
  
  minThrottle =    c(minThrottle, min(selectedLap$THROTTLE))
  minThrottle_S1 = c(minThrottle_S1, min(selectedLap$THROTTLE[which(selectedLap$LAP_DISTANCE_PERC < cut_s12)]))
  minThrottle_S2 = c(minThrottle_S2, min(selectedLap$THROTTLE[which((selectedLap$LAP_DISTANCE_PERC >= cut_s12) &
                                                                      (selectedLap$LAP_DISTANCE_PERC <= cut_s23))]))
  minThrottle_S3 = c(minThrottle_S3, min(selectedLap$THROTTLE[which(selectedLap$LAP_DISTANCE_PERC > cut_s23)]))
  
  aveThrottle =    c(aveThrottle, mean(selectedLap$THROTTLE))
  aveThrottle_S1 = c(aveThrottle_S1, mean(selectedLap$THROTTLE[which(selectedLap$LAP_DISTANCE_PERC < cut_s12)]))
  aveThrottle_S2 = c(aveThrottle_S2, mean(selectedLap$THROTTLE[which((selectedLap$LAP_DISTANCE_PERC >= cut_s12) &
                                                                       (selectedLap$LAP_DISTANCE_PERC <= cut_s23))]))
  aveThrottle_S3 = c(aveThrottle_S3, mean(selectedLap$THROTTLE[which(selectedLap$LAP_DISTANCE_PERC > cut_s23)]))
  
  timeDecreasingThrottle = c(timeDecreasingThrottle, 
                             sum(selectedLap[which(selectedLap$DECREASING_THROTTLE == TRUE),]$TIME_SINCE_LAST_FRAME))
  timeDecreasingThrottle_S1 = c(timeDecreasingThrottle_S1, 
                                sum(selectedLap[which(selectedLap$DECREASING_THROTTLE == TRUE &
                                                  selectedLap$LAP_DISTANCE_PERC < cut_s12),]$TIME_SINCE_LAST_FRAME))
  timeDecreasingThrottle_S2 = c(timeDecreasingThrottle_S2, 
                                sum(selectedLap[which(selectedLap$DECREASING_THROTTLE == TRUE &
                                                        (selectedLap$LAP_DISTANCE_PERC >= cut_s12) &
                                                        (selectedLap$LAP_DISTANCE_PERC <= cut_s23)),]$TIME_SINCE_LAST_FRAME))
  timeDecreasingThrottle_S3 = c(timeDecreasingThrottle_S3, 
                                sum(selectedLap[which(selectedLap$DECREASING_THROTTLE == TRUE &
                                                        selectedLap$LAP_DISTANCE_PERC > cut_s23),]$TIME_SINCE_LAST_FRAME))
  
  timeIncreasingThrottle = c(timeIncreasingThrottle, 
                             sum(selectedLap[which(selectedLap$INCREASING_THROTTLE == TRUE),]$TIME_SINCE_LAST_FRAME))
  timeIncreasingThrottle_S1 = c(timeIncreasingThrottle_S1, 
                                sum(selectedLap[which(selectedLap$INCREASING_THROTTLE == TRUE &
                                                        selectedLap$LAP_DISTANCE_PERC < cut_s12),]$TIME_SINCE_LAST_FRAME))
  timeIncreasingThrottle_S2 = c(timeIncreasingThrottle_S2, 
                                sum(selectedLap[which(selectedLap$INCREASING_THROTTLE == TRUE &
                                                        (selectedLap$LAP_DISTANCE_PERC >= cut_s12) &
                                                        (selectedLap$LAP_DISTANCE_PERC <= cut_s23)),]$TIME_SINCE_LAST_FRAME))
  timeIncreasingThrottle_S3 = c(timeIncreasingThrottle_S3, 
                                sum(selectedLap[which(selectedLap$INCREASING_THROTTLE == TRUE &
                                                        selectedLap$LAP_DISTANCE_PERC > cut_s23),]$TIME_SINCE_LAST_FRAME))
  
  timeSameThrottle = c(timeSameThrottle, 
                             sum(selectedLap[which(selectedLap$INCREASING_THROTTLE == FALSE & selectedLap$DECREASING_THROTTLE == FALSE),]$TIME_SINCE_LAST_FRAME))
  timeSameThrottle_S1 = c(timeSameThrottle_S1, 
                                sum(selectedLap[which(selectedLap$INCREASING_THROTTLE == FALSE &
                                                        selectedLap$DECREASING_THROTTLE == FALSE &
                                                        selectedLap$LAP_DISTANCE_PERC < cut_s12),]$TIME_SINCE_LAST_FRAME))
  timeSameThrottle_S2 = c(timeSameThrottle_S2, 
                                sum(selectedLap[which(selectedLap$INCREASING_THROTTLE == FALSE &
                                                        selectedLap$DECREASING_THROTTLE == FALSE &
                                                        (selectedLap$LAP_DISTANCE_PERC >= cut_s12) &
                                                        (selectedLap$LAP_DISTANCE_PERC <= cut_s23)),]$TIME_SINCE_LAST_FRAME))
  timeSameThrottle_S3 = c(timeSameThrottle_S3, 
                                sum(selectedLap[which(selectedLap$INCREASING_THROTTLE == FALSE &
                                                        selectedLap$DECREASING_THROTTLE == FALSE &
                                                        selectedLap$LAP_DISTANCE_PERC > cut_s23),]$TIME_SINCE_LAST_FRAME))
  
  
  
  firstBrakeDist = c(firstBrakeDist, selectedLap$LAP_DISTANCE_PERC[min(which(selectedLap$BRAKE != 0))])
  firstBrakeLateral = c(firstBrakeLateral, selectedLap$LATERAL[min(which(selectedLap$BRAKE != 0))])
  firstBrakeSpeed = c(firstBrakeSpeed, selectedLap$SPEED_KPH[min(which(selectedLap$BRAKE != 0))])
  
}

lapList["maxBrake"] = maxBrake
lapList["maxBrake_S1"] = maxBrake_S1
lapList["maxBrake_S2"] = maxBrake_S2
lapList["maxBrake_S3"] = maxBrake_S3

lapList["aveBrake"] = aveBrake
lapList["aveBrake_S1"] = aveBrake_S1
lapList["aveBrake_S2"] = aveBrake_S2
lapList["aveBrake_S3"] = aveBrake_S3

lapList["minThrottle"] = minThrottle
lapList["minThrottle_S1"] = minThrottle_S1
lapList["minThrottle_S2"] = minThrottle_S2
lapList["minThrottle_S3"] = minThrottle_S3

lapList["aveThrottle"] = aveThrottle
lapList["aveThrottle_S1"] = aveThrottle_S1
lapList["aveThrottle_S2"] = aveThrottle_S2
lapList["aveThrottle_S3"] = aveThrottle_S3

lapList["firstBrakeDist"] = firstBrakeDist
lapList["firstBrakeLateral"] = firstBrakeLateral
lapList["firstBrakeSpeed"] = firstBrakeSpeed

lapList["timeDecreasingThrottle"] = timeDecreasingThrottle
lapList["timeDecreasingThrottle_S1"] = timeDecreasingThrottle_S1
lapList["timeDecreasingThrottle_S2"] = timeDecreasingThrottle_S2
lapList["timeDecreasingThrottle_S3"] = timeDecreasingThrottle_S3

lapList["timeIncreasingThrottle"] = timeIncreasingThrottle
lapList["timeIncreasingThrottle_S1"] = timeIncreasingThrottle_S1
lapList["timeIncreasingThrottle_S2"] = timeIncreasingThrottle_S2
lapList["timeIncreasingThrottle_S3"] = timeIncreasingThrottle_S3

lapList["timeSameThrottle"] = timeSameThrottle
lapList["timeSameThrottle_S1"] = timeSameThrottle_S1
lapList["timeSameThrottle_S2"] = timeSameThrottle_S2
lapList["timeSameThrottle_S3"] = timeSameThrottle_S3



rm(maxBrake, maxBrake_S1, maxBrake_S2, maxBrake_S3,
   aveBrake, aveBrake_S1, aveBrake_S2, aveBrake_S3,
   minThrottle, minThrottle_S1, minThrottle_S2, minThrottle_S3,
   aveThrottle, aveThrottle_S1, aveThrottle_S2, aveThrottle_S3,
   timeDecreasingThrottle, timeDecreasingThrottle_S1, timeDecreasingThrottle_S2,
   timeDecreasingThrottle_S3, timeIncreasingThrottle, timeIncreasingThrottle_S1, 
   timeIncreasingThrottle_S2, timeIncreasingThrottle_S3, timeSameThrottle,
   timeSameThrottle_S1, timeSameThrottle_S2, timeSameThrottle_S3,
   firstBrakeDist,
   firstBrakeLateral,
   firstBrakeSpeed)

hist(lapList[1:50,]$firstBrakeDist, breaks = 100)





#PLOTTING BRAKE POINTS
plot(lapList$firstBrakeLateral, lapList$firstBrakeDist, cex = .1, ylim = c(.22,.39), xlim = c(0,1))
points(lapList$firstBrakeLateral[1:50], lapList$firstBrakeDist[1:50], col = "red")
points(lapList$firstBrakeLateral[51:200], lapList$firstBrakeDist[51:200], col = "blue")



plot(filtLeft$WORLDPOSX,filtLeft$WORLDPOSY, type = "l",asp = 1)
lines(filtRight$WORLDPOSX,filtRight$WORLDPOSY)
points(lap1$WORLDPOSX[which(lap1$THROTTLE == 1)],lap1$WORLDPOSY[which(lap1$THROTTLE == 1)], col = "red")
points(lap1$WORLDPOSX[which(lap1$THROTTLE != 1)],lap1$WORLDPOSY[which(lap1$THROTTLE != 1)], col = "blue")



plot(lap100$LAP_DISTANCE_PERC, lap100$BRAKE, type = "l")
lines(lap1$LAP_DISTANCE_PERC, lap1$BRAKE, col = "red")
lines(lap450$LAP_DISTANCE_PERC, lap450$BRAKE, col = "blue")

plot(lap100$LAP_DISTANCE_PERC, lap100$THROTTLE, type = "l")
lines(lap1$LAP_DISTANCE_PERC, lap1$THROTTLE, col = "red")
lines(lap450$LAP_DISTANCE_PERC, lap450$THROTTLE, col = "blue")

plot(lap1$LAP_DISTANCE_PERC, lap1$THROTTLE, type = "l")
lines(lap1$LAP_DISTANCE_PERC, lap1$BRAKE, col = "red")

plot(lapList$CURRENT_LAP_TIME_MS, lapList$firstBrakeLateral, ylim = c(0,1), xlim = c(12000,14000), cex = 0.001)
points(lapList$CURRENT_LAP_TIME_MS[1:50], lapList$firstBrakeLateral[1:50], col = "red")
points(lapList$CURRENT_LAP_TIME_MS[51:200], lapList$firstBrakeLateral[51:200], col = "blue")


plot(lap450$LAP_DISTANCE_PERC, lap450$STEERING, type = "l")
lines(lap1$LAP_DISTANCE_PERC, lap1$STEERING, col = "red")
lines(lap100$LAP_DISTANCE_PERC, lap100$STEERING, col = "blue")


max(lap1$BRAKE)

hist(firstBrakeDist, breaks = 100)
hist(firstBrakeLateral, breaks = 100)


plot(lap516$LAP_DISTANCE_PERC, lap516$BRAKE,asp = 1)
points(lap1$LAP_DISTANCE_PERC, lap1$BRAKE, col = "red")

plot(lap516$WORLDPOSX, lap516$WORLDPOSY, type = "l", asp = 1, xlim = c(320,410), ylim = c(100,250))
lines(filtLeft$WORLDPOSX, filtLeft$WORLDPOSY, col = "red")
lines(filtRight$WORLDPOSX, filtRight$WORLDPOSY, col = "blue")
lines(lap1$WORLDPOSX, lap1$WORLDPOSY, col = "purple")

plot(lap516$LAP_DISTANCE_PERC, lap516$LATERAL)

plot(lap1$STEERING, lap1$LAP_DISTANCE_PERC, type = "l", xlim = c(-1,1))
lines(lap100$STEERING, lap100$LAP_DISTANCE_PERC, col = 'red')

plot(filtCars$LAP_DISTANCE_PERC, filtCars$STEERING, type = "l")
lines(lap1$LAP_DISTANCE_PERC, lap1$STEERING , col = 'red')


plot(filtCars$LAP_DISTANCE_PERC, filtCars$BRAKE, type = "l", cex = 0.01)
lines(lap516$LAP_DISTANCE_PERC,lap516$BRAKE, col = "green")
lines(lap1$LAP_DISTANCE_PERC,lap1$BRAKE, col = "red")
lines(lap250$LAP_DISTANCE_PERC,lap250$BRAKE, col = "blue")

model = lm(lapList$CURRENT_LAP_TIME_MS ~ lapList$firstBrakeDist + lapList$firstBrakeLateral + lapList$maxBrake + lapList$firstBrakeSpeed)

summary(model)

