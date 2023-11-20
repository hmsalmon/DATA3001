#THIS FILE CREATES A TRAINING DATA SET USING THE FOLLOWING FILES

#constructingTrainTestSetv1.0
#constructingTrackSidesv1.1
#projectDataCleaningv1.5
#findStart$Finishv2.1

#These files will produce the csvs used below

#NEEDS DPLYR PACKAGE

filtCars = read_csv("filtCars_inRegion.csv")
filtCars2 = read_csv("filtCars_inTimedRange.csv")

filtLeft = read_csv("filtLeft.csv")
filtRight = read_csv("filtRight.csv")


#TABLE WITH LIST OF ALL LAPS WITH SECTOR TIME IN "CURRENT_LAP_TIME_MS" COLUMN
lapList = 
  
  filtCars2 %>%
  group_by(SESSION_IDENTIFIER, LAP_NUM) %>%
  slice(which.max(NORMALISED_LAP_TIME_MS))

lapList = arrange(lapList, NORMALISED_LAP_TIME_MS)

lapList = subset(lapList, select = c(SESSION_IDENTIFIER, LAP_NUM, CURRENT_LAP_TIME_MS, NORMALISED_LAP_TIME_MS))

#TEST HISTOGRAM
hist(lapList$NORMALISED_LAP_TIME_MS, breaks = 100)

#EXTRACT DATA FOR LAP WITH THE NTH FASTEST TIME FOR RANGE
findLap <- function(rank) {
  
  sel_sessionid = lapList$SESSION_IDENTIFIER[rank]
  sel_lapnumid = lapList$LAP_NUM[rank]
  
  return(filtCars[filtCars$SESSION_IDENTIFIER == sel_sessionid & filtCars$LAP_NUM == sel_lapnumid,])
  
}
test = c()
steeringfunc <- function(data) {
  
  totalval = 0
  
  print(nrow(data))
  
  for (i in 1:(length(data$SESSION_IDENTIFIER) - 1)) {
    
    distbetweenPoints = data$LAP_DISTANCE_PERC[i+1] - data$LAP_DISTANCE_PERC[i]
    
    curVal = abs(data$STEERING[i])
    nextVal = abs(data$STEERING[i+1])
    
    trapVal = (curVal+nextVal)*distbetweenPoints/2
    
    totalval = totalval + trapVal
    
    test = c(test,totalval)
    
    print(i)
    print(totalval)
  }
  
  startAdjust = abs(data$LAP_DISTANCE_PERC[1]*data$STEERING[1])
  endAdjust = abs((1-data$LAP_DISTANCE_PERC[i+1])*data$STEERING[i+1])
  
  totalval = totalval + startAdjust + endAdjust
  
  cat("startadj: ",startAdjust)
  cat("endadj: ",endAdjust)
  cat("total: ",totalval)
  #rm(i,curVal, nextVal, trapVal, distBetweenPoints, startAdjust, endAdjust)
  return(totalval)
  
}

g = steeringfunc(sl)

testSteer = c()

for(i in 1:length(lapList$SESSION_IDENTIFIER)) {

  print(i)
  testSteer = c(testSteer, steeringfunc(findLap(i)))

}
plot(seq(1,842),testSteer)



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

validLap = c()
validLap_4wheels = c()

firstBrakeDist = c()
firstBrakeLateral = c()
firstBrakeSpeed = c()

steeringIntensity = c()
steeringIntensity_S1 = c()
steeringIntensity_S2 = c()
steeringIntensity_S3 = c()

#Sector 1 = 0-20% of track
#Sector 2 = 20-60% of track
#Sector 3 = 60-100% of track

cut_s12 = 0.2
cut_s23 = 0.6



for (i in 1:length(lapList$SESSION_IDENTIFIER)) {
  
  sl = findLap(i)
  
  sl_S1 = sl[which(sl$LAP_DISTANCE_PERC < cut_s12),]
  sl_S2 = sl[which((sl$LAP_DISTANCE_PERC >= cut_s12) & (sl$LAP_DISTANCE_PERC <= cut_s23)),]
  sl_S3 = sl[which(sl$LAP_DISTANCE_PERC > cut_s23),]
    
    
  maxBrake =    c(maxBrake, max(sl$BRAKE))
  maxBrake_S1 = c(maxBrake_S1, max(sl_S1$BRAKE))
  maxBrake_S2 = c(maxBrake_S2, max(sl_S2$BRAKE))
  maxBrake_S3 = c(maxBrake_S3, max(sl_S3$BRAKE))

  aveBrake =    c(aveBrake, mean(sl$BRAKE))
  aveBrake_S1 = c(aveBrake_S1, mean(sl_S1$BRAKE))
  aveBrake_S2 = c(aveBrake_S2, mean(sl_S2$BRAKE))
  aveBrake_S3 = c(aveBrake_S3, mean(sl_S3$BRAKE))

  minThrottle =    c(minThrottle, min(sl$THROTTLE))
  minThrottle_S1 = c(minThrottle_S1, min(sl_S1$THROTTLE))
  minThrottle_S2 = c(minThrottle_S2, min(sl_S2$THROTTLE))
  minThrottle_S3 = c(minThrottle_S3, min(sl_S3$THROTTLE))

  aveThrottle =    c(aveThrottle, mean(sl$THROTTLE))
  aveThrottle_S1 = c(aveThrottle_S1, mean(sl_S1$THROTTLE))
  aveThrottle_S2 = c(aveThrottle_S2, mean(sl_S2$THROTTLE))
  aveThrottle_S3 = c(aveThrottle_S3, mean(sl_S3$THROTTLE))


  firstBrakeDist = c(firstBrakeDist, sl$LAP_DISTANCE_PERC[min(which(sl$BRAKE != 0))])
  firstBrakeLateral = c(firstBrakeLateral, sl$LATERAL[min(which(sl$BRAKE != 0))])
  firstBrakeSpeed = c(firstBrakeSpeed, sl$SPEED_KPH[min(which(sl$BRAKE != 0))])

  validLap = c(validLap, min(sl$OnTrack))
  validLap_4wheels = c(validLap_4wheels, min(sl$FourWheelsOn))
  
  steeringIntensity = c(steeringIntensity, steeringfunc(sl))
  steeringIntensity_S1 = c(steeringIntensity_S1, steeringfunc(sl_S1))
  steeringIntensity_S2 = c(steeringIntensity_S2, steeringfunc(sl_S2))
  steeringIntensity_S3 = c(steeringIntensity_S3, steeringfunc(sl_S3))
  
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

lapList["ValidLap"] = validLap
lapList["ValidLap_4wheels"] = validLap_4wheels

lapList["firstBrakeDist"] = firstBrakeDist
lapList["firstBrakeLateral"] = firstBrakeLateral
lapList["firstBrakeSpeed"] = firstBrakeSpeed

lapList["steeringIntensity"] = steeringIntensity
lapList["steeringIntensity_S1"] = steeringIntensity_S1
lapList["steeringIntensity_S2"] = steeringIntensity_S2
lapList["steeringIntensity_S3"] = steeringIntensity_S3

rm(maxBrake, maxBrake_S1, maxBrake_S2, maxBrake_S3,
   aveBrake, aveBrake_S1, aveBrake_S2, aveBrake_S3,
   minThrottle, minThrottle_S1, minThrottle_S2, minThrottle_S3,
   aveThrottle, aveThrottle_S1, aveThrottle_S2, aveThrottle_S3,
   validLap, validLap_4wheels,
   firstBrakeDist,
   firstBrakeLateral,
   firstBrakeSpeed,
   steeringIntensity, steeringIntensity_S1, steeringIntensity_S2, steeringIntensity_S3)

rm(sl)

print("RUN SUCCESSFULLY: constructingVariablesv1.0.R")

#write_csv(lapList, "trainingDatav2.0.csv")
