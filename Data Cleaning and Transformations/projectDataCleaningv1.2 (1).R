getwd()
library(dplyr) #INSTALL THIS PACKAGE IF YOU HAVENT GOT IT ALREADY

#LOAD IN FILES (SET WORKING DIRECTORY TO FOLDER WITH THESE FILES IN IT)
turns = read.csv("f1sim-ref-turns.csv")
right = read.csv("f1sim-ref-right.csv")
left = read.csv("f1sim-ref-left.csv")
track = read.csv("f1sim-ref-line.csv")
cars = read.csv("f1sim-data-2023.csv")

#HELPER FUNCTIONS WRITTEN HERE
badDataFlag <- function(toTestSIDs,toTestLIDs,badSIDs,badLIDs){
  
  flags = c()
  
  for (i in 1:length(toTestSIDs)) {
    #print(i)
    if (toTestSIDs[i] %in% badSIDs) {
      #print("session id bad")
      rowId = which(badSIDs == toTestSIDs[i])
      #print(rowId)
      if(toTestLIDs[i] %in% badLIDs[rowId]){
        #print("bad lapID")
        flags = append(flags, 1)
        
      } else {
        #print("good lapID")
        flags = append(flags, 0)
        
      }
      
    } else {
      #print("good session ID")
      flags = append(flags, 0)
      
    }
  }
  
  return(flags)
  
}
distanceFrom <- function(changingPointsX, fixedPointX, changingPointsY, fixedPointY){
  
  
  return(sqrt((changingPointsX - fixedPointX)*(changingPointsX - fixedPointX)+(changingPointsY - fixedPointY)*(changingPointsY - fixedPointY)))
  
}
isCloserThan2 <- function(testDists, markerDist) {
  #print("in function")
  
  
  return(testDists <= markerDist)
}


# Finds point (x,y) on a line segment defined by Coord1 and Coord2 perpendicular 
# to another point Point

perpPoint <- function(Coord1, Coord2, Point) {
  k = ((Coord2[2]-Coord1[2]) * (Point[1]-Coord1[1]) - (Coord2[1]-Coord1[1]) * (Point[2]-Coord1[2])) / ((Coord2[2]-Coord1[2])^2 + (Coord2[1]-Coord1[1])^2)
  x = Point[1] - k * (Coord2[2]-Coord1[2])
  y = Point[2] + k * (Coord2[1]-Coord1[1])
  perpPoint <- c(x, y)
}


# CONSTRUCTING THE STARTLINE AND FINISHLINE

# Finding Start Line Coordinates:

# we start with the fifth frame on the left side of the track
leftStart = filtLeft[which(filtLeft$LAP_DISTANCE_PERC == 0),]
leftStartCoord = c(leftStart$WORLDPOSX, leftStart$WORLDPOSY)

# calculate the point on the right side of the track that is perpendicular to
# our left starting point

right5frame = filtRight[which(filtRight$LAP_DISTANCE_PERC == 0),]
rightTrackSegment1 = filtRight[which(filtRight$FRAME == right5frame$FRAME - 4),]
rightTrackSegement2 = filtRight[which(filtRight$FRAME == right5frame$FRAME + 4),]
Coord1 = c(rightTrackSegment1$WORLDPOSX, rightTrackSegment1$WORLDPOSY)
Coord2 = c(rightTrackSegement2$WORLDPOSX, rightTrackSegement2$WORLDPOSY)

rightStartCoord = perpPoint(Coord1, Coord2, leftStartCoord)

x_start = c(leftStartCoord[1], rightStartCoord[1])
y_start = c(leftStartCoord[2], rightStartCoord[2])


# Finding Finish Line Coordinates

# we start with a designated finishing point on the left side of the track
leftFinish = filtLeft[which(filtLeft$LAP_DISTANCE_PERC == 1),]
leftFinishCoord = c(leftFinish$WORLDPOSX, leftFinish$WORLDPOSY)


# calculate the point on the right side of the track that is perpendicular to
# our left finishing point

rightFinishFrame = filtRight[which(filtRight$LAP_DISTANCE_PERC == 1),]
rightTrackSegment1 = filtRight[which(filtRight$FRAME == rightFinishFrame$FRAME - 4),]
rightTrackSegement2 = filtRight[which(filtRight$FRAME == rightFinishFrame$FRAME + 4),]
Coord1 = c(rightTrackSegment1$WORLDPOSX, rightTrackSegment1$WORLDPOSY)
Coord2 = c(rightTrackSegement2$WORLDPOSX, rightTrackSegement2$WORLDPOSY)

rightFinishCoord = perpPoint(Coord1, Coord2, leftFinishCoord)
x_finish = c(leftFinishCoord[1], rightFinishCoord[1])
y_finish = c(leftFinishCoord[2], rightFinishCoord[2])


# Obtain Equations of Start and Finish Line

start_line_intercept = summary(lm(y_start ~ x_start))$coefficients[1,1]
start_line_gradient = summary(lm(y_start ~ x_start))$coefficients[2,1]

finish_line_intercept = summary(lm(y_finish ~ x_finish))$coefficients[1,1]
finish_line_gradient = summary(lm(y_finish ~ x_finish))$coefficients[2,1]

# Check which frames are above or below the start and finish line

cars$AboveStart <- (cars$WORLDPOSY > start_line_intercept + start_line_gradient * cars$WORLDPOSX)
cars$AboveFinish <- (cars$WORLDPOSY > finish_line_intercept + finish_line_gradient * cars$WORLDPOSX)


#FINDING ROWS WITHOUT POSITIONAL DATA
cars$c1dist <- sqrt((cars$WORLDPOSX - turns$APEX_X1[1])^2+(cars$WORLDPOSY - turns$APEX_Y1[1])^2)
rowsWithoutData = which(is.na(cars$c1dist))

#need to use next three rows to remove entire lap if a point in that lap doesnt have data?
#maybe this is not necessary. Will think about it.
badData <- cars[rowsWithoutData,]
badDataSessionIDs <- badData$SESSION_IDENTIFIER
badDataLapIDs <- badData$LAP_NUM

#REMOVING DATA POINTS WITHOUT POSITIONAL DATA
cleanCars <- cars[-rowsWithoutData,]

#FILTER DATA TO ONLY TURNS 1 AND 2
filtCars = cleanCars[which(cleanCars$AboveStart == FALSE & cleanCars$AboveFinish == TRUE & cleanCars$SECTOR == 0),]

#PLOT FOR SANTIY CHECK OF POINTS
plot(filtCars$WORLDPOSX, filtCars$WORLDPOSY, cex = 0.01)

#CREATE DATAFRAME TO TEST ON
testCars = filtCars

shortPoint = c()
shortPoint2 = c()
shortPointR = c()
shortPointR2 = c()

#THIS LOOP IDENTIFIES THE NEAREST AND 2ND NEAREST POINTS TO EACH CAR LOCATION
#ON THE LEFT AND RIGHT SIDE OF THE TRACK
for (i in 1:length(testCars$WORLDPOSX)) {
  
  dists2 = c()
  dists2R = c()
  ithdist = distanceFrom(filtLeft$WORLDPOSX, rep(testCars$WORLDPOSX[i], length(filtLeft$WORLDPOSX)),
                         filtLeft$WORLDPOSY, rep(testCars$WORLDPOSY[i], length(filtLeft$WORLDPOSX)))
  ithdistR = distanceFrom(filtRight$WORLDPOSX, rep(testCars$WORLDPOSX[i], length(filtRight$WORLDPOSX)),
                          filtRight$WORLDPOSY, rep(testCars$WORLDPOSY[i], length(filtRight$WORLDPOSX)))
  dists2 = c(dists2, ithdist)
  dists2R = c(dists2R, ithdistR)
  
  #NEAREST POINTS
  shortPoint = c(shortPoint, which.min(dists2))
  shortPointR = c(shortPointR, which.min(dists2R))
  
  #REMOVE NEAREST POINTS
  dists2[which.min(dists2)] = max(dists2)
  dists2R[which.min(dists2R)] = max(dists2R)
  
  #SECOND NEAREST POINTS
  shortPoint2 = c(shortPoint2, which.min(dists2))
  shortPointR2 = c(shortPointR2, which.min(dists2R))
} #THIS LOOP WILL TAKE ABOUT 2M 30S TO COMPLETE

#PLACES THE TRACK FRAME ALREADY PASSED IN ONE VECTOR AND THE APPROACHING FRAME IN ANOTHER
firstNearTrackPoint <- function(point1,point2){
  
  return(pmin(point1,point2))
  
}
secondNearTrackPoint <- function(point1,point2){
  
  return(pmax(point1,point2))
  
}

firstTrack = firstNearTrackPoint(shortPoint,shortPoint2)
secondTrack = secondNearTrackPoint(shortPoint,shortPoint2)
firstTrackR = firstNearTrackPoint(shortPointR,shortPointR2)
secondTrackR = secondNearTrackPoint(shortPointR,shortPointR2)

#RETURNS COORDINATES OF LEFT SIDE OF TRACK
leftTrackPointCoordX <- function(points){
  
  return(filtLeft[points,]$WORLDPOSX)
  
}
leftTrackPointCoordY <- function(points){
  
  return(filtLeft[points,]$WORLDPOSY)
  
}

#RETURNS COORDINATES OF RIGHT SIDE OF TRACK
RightTrackPointCoordX <- function(points){
  
  return(filtRight[points,]$WORLDPOSX)
  
}
RightTrackPointCoordY <- function(points){
  
  return(filtRight[points,]$WORLDPOSY)
  
}

#COORDINATES OF MOST TRACK FRAME THAT WAS MOST RECENTLY PAST
leftX = leftTrackPointCoordX(firstTrack)
leftY = leftTrackPointCoordY(firstTrack)

RightX = RightTrackPointCoordX(firstTrackR)
RightY = RightTrackPointCoordY(firstTrackR)

#STRAIGHT LINE DISTANCE BETWEEN CAR LOCATION AND MOST RECENT TRACK FRAME
distBetweenPoints <- function(trackX,trackY,carPosX,carPosY) {
  
  
  return(sqrt((trackX - carPosX)*(trackX - carPosX) + (trackY - carPosY)*(trackY - carPosY)))
  
}

dist_nearPoint_car = distBetweenPoints(leftX, leftY, testCars$WORLDPOSX, testCars$WORLDPOSY)
dist_nearPoint_carR = distBetweenPoints(RightX, RightY, testCars$WORLDPOSX, testCars$WORLDPOSY)

#FUNCTION RETURNS DATA FRAME WITH COEFFICENTS OF LINEAR EQUATION BETWEEN FRAMES OF TRACK
lineEquation <- function(x1,y1,x2,y2) {
  
  m = (y2-y1)/(x2-x1)
  
  A = m
  B = -1
  C = -x1*m + y1
  
  df = data.frame(A,B,C)
  
  colnames(df) = c("A","B","C")
  
  return(df)
  
}

lineL = lineEquation( filtLeft$WORLDPOSX,filtLeft$WORLDPOSY,c(filtLeft$WORLDPOSX[-1],0),c(filtLeft$WORLDPOSY[-1],0))
lineR = lineEquation( filtRight$WORLDPOSX,filtRight$WORLDPOSY,c(filtRight$WORLDPOSX[-1],0),c(filtRight$WORLDPOSY[-1],0))

#PERPENDICULAR DISTANCE BETWEEN CAR LOCATIONS AND TRACK LINE
perpDistLeft2 <- function(x1,y1,line){
  
  trueDist = abs(line$A*x1+line$B*y1+line$C)/sqrt(line$A*line$A+line$B*line$B)
  
  offTrackTest = sign((line$B*y1+line$C)/(-line$A) - x1)
  
  return(trueDist*offTrackTest)
  
}
perpDistRight2 <- function(x1,y1,line){
  
  trueDist = abs(line$A*x1+line$B*y1+line$C)/sqrt(line$A*line$A+line$B*line$B)
  
  offTrackTest = sign(x1 - (line$B*y1 +line$C)/(-line$A))
  
  return(trueDist*offTrackTest)
  
}

distLeftEdge2 = perpDistLeft2(testCars$WORLDPOSX,testCars$WORLDPOSY,lineL[firstTrack,])
distRightEdge2 = perpDistRight2(testCars$WORLDPOSX,testCars$WORLDPOSY,lineR[firstTrackR,])

#RATIO 
trackPosition2 = distLeftEdge2/(distLeftEdge2+distRightEdge2)

testCars["LATERAL"] = trackPosition2


#Distance from first track point to projected car point on track line
partialDist <- function(perp_dist,first_dist) {
  
  return(sqrt((first_dist*first_dist) - (perp_dist*perp_dist)))
  
}

lapDistForward = filtLeft$LAP_DISTANCE_TOTAL[firstTrack] + partialDist(distLeftEdge2,dist_nearPoint_car)

testCars["LAP_DISTANCE_TOTAL"] = lapDistForward
testCars["LAP_DISTANCE_PERC"] = testCars$LAP_DISTANCE / filtLeft$LAP_DISTANCE_TOTAL[length(filtLeft$LAP_DISTANCE_TOTAL) - 1]

#LET ORGINAL VARAIBLE = TEST VARIABLE AS IT WORKS
filtCars = testCars

#TABLE WITH LIST OF ALL LAPS WITH SECTOR TIME IN "CURRENT_LAP_TIME_MS" COLUMN
lapList = 
  
  filtCars %>%
  group_by(SESSION_IDENTIFIER, LAP_NUM) %>%
  slice(which.max(CURRENT_LAP_TIME_MS))

lapList = arrange(lapList, CURRENT_LAP_TIME_MS)

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
lap100 = findLap(100)
lap250 = findLap(250)
lap517 = findLap(517)

plot(lap3$LAP_DISTANCE_PERC ,lap3$LATERAL, type = "l")
lines(lap1$LAP_DISTANCE_PERC, lap1$LATERAL, col = "skyblue")
# lines(lap1$LAP_DISTANCE_PERC,lap1$BRAKE, col = "red")
# lines(lap3$LAP_DISTANCE_PERC,lap3$BRAKE, col = "green")
# lines(lap1$LAP_DISTANCE_PERC,rep(1,length(lap1$BRAKE)), col = "blue")
# lines(lap1$LAP_DISTANCE_PERC,rep(0,length(lap1$BRAKE)), col = "blue")

#COMPARING THE PATH TAKEN IN DIFFERENT LAPS
plot(lap1$WORLDPOSX, lap1$WORLDPOSY, type = "l", col = "blue", asp = 1)
lines(lap250$WORLDPOSX, lap250$WORLDPOSY, col = "grey")
#lines(lap2$WORLDPOSX, lap2$WORLDPOSY, col = "red")
#lines(lap3$WORLDPOSX, lap3$WORLDPOSY, col = "orange")
lines(lap100$WORLDPOSX, lap100$WORLDPOSY, col = "cyan")


#THROTTLE COMPARISON PLOTS
plot(lap1$LAP_DISTANCE, lap1$THROTTLE, type = "l")
lines(lap2$LAP_DISTANCE, lap2$THROTTLE, col = "red")

#BRAKE COMPARISON PLOTS
plot(lap1$LAP_DISTANCE, lap1$BRAKE, type = "l")
lines(lap100$LAP_DISTANCE, lap100$BRAKE, col = "red")

#THROTTLE AND BRAKE ON SAME PLOT
plot(lap1$LAP_DISTANCE, lap1$THROTTLE, type = "l")
lines(lap1$LAP_DISTANCE, lap1$BRAKE, col = "red")

plot(lap517$LAP_DISTANCE, lap517$THROTTLE, type = "l")
lines(lap517$LAP_DISTANCE, lap517$BRAKE, col = "red")

#SPEED COMPARISON
plot(lap1$LAP_DISTANCE, lap1$SPEED_KPH, type = "l")
lines(lap2$LAP_DISTANCE, lap2$SPEED_KPH, col = "red")
lines(lap100$LAP_DISTANCE, lap100$SPEED_KPH, col = "blue")

plot(lap2$LAP_DISTANCE, lap2$THROTTLE, type = "l", col = "blue")



#TRACK MAP
plot(lap3$WORLDPOSX[85:125], lap3$WORLDPOSY[85:125], cex = 0.15, asp = 1) #FASTEST LAP PATH
points(left$WORLDPOSX, left$WORLDPOSY, cex = 0.1, col = "red") #LEFT EDGE OF TRACK
points(right$WORLDPOSX, right$WORLDPOSY, cex = 0.1, col = "blue") #RIGHT EDGE OF TRACK
points(turns$APEX_X1,turns$APEX_Y1, cex = 0.5, col = "orange") #APEX OF CORNERS
points(lap250$WORLDPOSX, lap250$WORLDPOSY, cex = 0.15, col = "brown") #PATH OF 250TH FASTEST LAP
points(filtCars$WORLDPOSX,filtCars$WORLDPOSY, cex = 0.01) #PATH OF ALL LAPS

#SANITY CHECK FOR SELECTED LAP. PLOT EVERY LAP AND OBSERVE ANY MISSING DATA

for (i in 1:517) {
  lap = findLap(i)
  plot(filtLeft$WORLDPOSX, filtLeft$WORLDPOSY, cex = 0.1)
  points(filtRight$WORLDPOSX, filtRight$WORLDPOSY, cex = 0.1)
  lines(x = x_start,y=y_start,col = "red", lwd=5)
  lines(x = x_finish,y=y_finish,col = "red", lwd=5)
  points(lap$WORLDPOSX, lap$WORLDPOSY, col = 'green', cex = 0.1)
  title(main = paste("Lap", i))
}

# Notice that lap 17 and 19 have missing data - drop these laps

lap17 = findLap(17)
lap19 = findLap(19)

filtCars = filtCars[!(filtCars$SESSION_IDENTIFIER == unique(lap17$SESSION_IDENTIFIER) &
                        filtCars$LAP_NUM == unique(lap17$LAP_NUM)),]
              
filtCars = filtCars[!(filtCars$SESSION_IDENTIFIER == unique(lap19$SESSION_IDENTIFIER) &
                          filtCars$LAP_NUM == unique(lap19$LAP_NUM)),]

