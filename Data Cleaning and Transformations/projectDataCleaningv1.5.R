
#library(dplyr) #INSTALL THIS PACKAGE IF YOU HAVENT GOT IT ALREADY


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


# #THIS IS CLARIFYING THE DISCREPANCY BETWEEN LAP_DISTANCE_PERC AND ABOVEFINISH.
# #THE ROUNDING ERROR INVOLVED IN USING LINE EQUATIONS MISCATAGORISES ONE POINT
# (-223.956 > finish_line_intercept + finish_line_gradient*638.1987)
# 
# ll = lineEquation(x_finish[1],y_finish[1],x_finish[2],y_finish[2])
# 
# ll$A[1]*638.1987 + ll$B[1]*-223.956 + ll$C[1]
# 
# distBetweenPoints(filtLeft$WORLDPOSX[783],filtLeft$WORLDPOSY[783],638.1987,-223.956)
# distBetweenPoints(filtLeft$WORLDPOSX[781],filtLeft$WORLDPOSY[781],638.1987,-223.956)

#PREPARING DATA TO BE FILTERED (Finding distances to corners 1 and 2)
cars$c1dist <- sqrt((cars$WORLDPOSX - turns$APEX_X1[1])^2+(cars$WORLDPOSY - turns$APEX_Y1[1])^2)
cars$c2dist <- sqrt((cars$WORLDPOSX - turns$APEX_X1[2])^2+(cars$WORLDPOSY - turns$APEX_Y1[2])^2)
slDistToApexOne <- sqrt((rightFinishCoord[1] - turns$APEX_X1[1])^2 + (rightFinishCoord[2] - turns$APEX_Y1[1])^2)
halfwayDist2and3 = sqrt((turns$APEX_X1[2] - turns$APEX_X1[3])^2+(turns$APEX_X1[2] - turns$APEX_Y1[3])^2)/2

#FINDING ROWS WITHOUT POSITIONAL DATA
rowsWithoutData = which(is.na(cars$c1dist))

#need to use next three rows to remove entire lap if a point in that lap doesnt have data?
#maybe this is not necessary. Will think about it.
badData <- cars[rowsWithoutData,]
badDataSessionIDs <- badData$SESSION_IDENTIFIER
badDataLapIDs <- badData$LAP_NUM

#REMOVING DATA POINTS WITHOUT POSITIONAL DATA
cleanCars <- cars[-rowsWithoutData,]

#CREATE FLAG FOR WHETHER WE ARE INTERESTED IN THE POINT (Points near turns 1 and 2)
cleanCars$closeToOne <- isCloserThan2(cleanCars$c1dist, rep(slDistToApexOne, length(cleanCars$c1dist)))
cleanCars$closeToTwo <- isCloserThan2(cleanCars$c2dist, rep(halfwayDist2and3 + 10, length(cleanCars$c2dist)))
cleanCars$inRange <- (cleanCars$closeToOne | cleanCars$closeToTwo) & cleanCars$SECTOR == 0

#FILTER DATA TO ONLY TURNS 1 AND 2
filtCars = cleanCars[which(cleanCars$inRange == TRUE),]

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

distofftrack <- function(left, right) {
  
  offl = (left < 0)
  offr = (right < 0)
  
  leftadj = offl * left
  rightadj = offr * right
  
  return(abs(leftadj+rightadj))
  
}


distofftrackk = distofftrack(distLeftEdge2, distRightEdge2)

#Distance from first track point to projected car point on track line
partialDist <- function(perp_dist,first_dist) {
  
  return(sqrt((first_dist*first_dist) - (perp_dist*perp_dist)))
  
}

lapDistForward = filtLeft$LAP_DISTANCE_TOTAL[firstTrack] + partialDist(distLeftEdge2,dist_nearPoint_car)

testCars["LAP_DISTANCE_TOTAL"] = lapDistForward
testCars["LAP_DISTANCE_PERC"] = testCars$LAP_DISTANCE_TOTAL / filtLeft$LAP_DISTANCE_TOTAL[length(filtLeft$LAP_DISTANCE_TOTAL) - 4]


testCars$AboveStart2 = (testCars$LAP_DISTANCE_PERC < 0)
testCars$AboveFinish2 = (testCars$LAP_DISTANCE_PERC <= 1)


validCarPosition <- function(lateralPos) {
  
  
  return((lateralPos >= 0 & lateralPos <= 1))
  
  
}

validCarPosition2 <- function(lateralPos) {
  
  
  return((lateralPos < 1))
  
  
}

testCars["OnTrack"] = validCarPosition(testCars$LATERAL)
testCars["FourWheelsOn"] = validCarPosition2(distofftrackk)

testCars = subset(testCars, select = -c(c1dist,c2dist,closeToOne,closeToTwo,inRange))


#LET ORGINAL VARAIBLE = TEST VARIABLE AS IT WORKS
filtCars = testCars

rm(badDataLapIDs, badDataSessionIDs, dist_nearPoint_car, dist_nearPoint_carR, distLeftEdge2,
   distRightEdge2, dists2, dists2R, firstTrack, firstTrackR, halfwayDist2and3, i, ithdist,
   ithdistR, lapDistForward, leftX, leftY, RightX, RightY, rowsWithoutData, secondTrack, secondTrackR,
   shortPoint, shortPoint2, shortPointR, shortPointR2, slDistToApexOne,
   trackPosition2, Coord1, Coord2, finish_line_gradient, finish_line_intercept,
   start_line_gradient, start_line_intercept, x_finish, x_start, y_finish, y_start)

rm(badData, cleanCars, lineL, lineR, testCars, leftFinish, leftStart, right5frame,
   rightFinishFrame, rightTrackSegement2, rightTrackSegment1)
rm(cars, turns)



#THIS FUNCTION PLOTS THE PATH OF A CAR FOR GIVEN LAP
plotlap <- function(data,i) {
  
  plot(filtLeft$WORLDPOSX, filtLeft$WORLDPOSY, cex = 0.1,asp = 1)#, xlim = c(98,150), ylim = c(400,500))
  points(filtRight$WORLDPOSX, filtRight$WORLDPOSY, cex = 0.1)
  # lines(x = x_start,y=y_start,col = "red", lwd=5)
  # lines(x = x_finish,y=y_finish,col = "red", lwd=5)
  points(data$WORLDPOSX, data$WORLDPOSY, col = 'red', cex = 1)
  title(main = paste("Lap",i))
  
  
}


#TABLE WITH LIST OF ALL LAPS WITH SECTOR TIME IN "CURRENT_LAP_TIME_MS" COLUMN
lapList =
  
  filtCars %>%
  group_by(SESSION_IDENTIFIER, LAP_NUM) %>%
  slice(which.max(CURRENT_LAP_TIME_MS))

lapList = arrange(lapList, CURRENT_LAP_TIME_MS)

dpts = c()

for (i in 1:length(lapList$SESSION_IDENTIFIER)){
  
  session = lapList$SESSION_IDENTIFIER[i]
  lap = lapList$LAP_NUM[i]
  dpts = c(dpts, nrow(filtCars[which(filtCars$SESSION_IDENTIFIER == session & filtCars$LAP_NUM == lap),]))
  
}
lapList$ndatapoints = dpts

#EXTRACT DATA FOR LAP WITH THE NTH FASTEST TIME FOR RANGE
findLap <- function(rank) {

  sel_sessionid = lapList$SESSION_IDENTIFIER[rank]
  sel_lapnumid = lapList$LAP_NUM[rank]

  return(filtCars[filtCars$SESSION_IDENTIFIER == sel_sessionid & filtCars$LAP_NUM == sel_lapnumid,])

}

#DETERMINING THE LAPS SUITABLE FOR USE
columns = c("session","lapnum")
goodlaps = data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(goodlaps) = columns

for (i in 1:nrow(lapList)) {
  lap = findLap(i)
  if(nrow(lap) > 220) {
    #plotlap(lap,i)
    goodlaps[nrow(goodlaps)+1,] = c(lap$SESSION_IDENTIFIER[1],lap$LAP_NUM[1])
    
  }
}

filtCarsOriginal = filtCars
keeprows <- function(alldata, goodlapdata){
  
  keep = c()
  
  for (i in 1:length(alldata$SESSION_IDENTIFIER)) {
    
    if(alldata$SESSION_IDENTIFIER[i] %in% goodlapdata$session) {
      
      if(alldata$LAP_NUM[i] %in% goodlapdata$lapnum[which(goodlapdata$session == alldata$SESSION_IDENTIFIER[i])]) {
        
        keep = c(keep, 1)
      } else {
        
        keep = c(keep, 0)
        }
      } else {
        
        keep = c(keep, 0)
        
      }
    
    
    }
    
    return(keep)
  
}
testCars = filtCars
testCars$keep = keeprows(testCars,goodlaps)
testCars = testCars[which(testCars$keep == 1),]
filtCars = testCars

#CALCULATING NUMBER OF DATA POINTS PER LAP
dpts = c()
for (i in 1:length(lapList$SESSION_IDENTIFIER)){
  
  session = lapList$SESSION_IDENTIFIER[i]
  lap = lapList$LAP_NUM[i]
  dpts = c(dpts, nrow(filtCars[which(filtCars$SESSION_IDENTIFIER == session & filtCars$LAP_NUM == lap),]))
  
}
lapList$ndatapoints = dpts

lapList = lapList[which(lapList$ndatapoints != 0),]


rm(filtCarsOriginal, goodlaps, testCars)
rm(columns, dpts, i, lap, session)

print("RUN SUCCESSFULLY: projectDataCleaningv1.5.R")
