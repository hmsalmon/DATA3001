

#LOAD IN FILES (SET WORKING DIRECTORY TO FOLDER WITH THESE FILES IN IT)
turns = read.csv("f1sim-ref-turns.csv")
right = read.csv("f1sim-ref-right.csv")
left = read.csv("f1sim-ref-left.csv")
#cars = read.csv("f1sim-data-2023.csv")

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

#FINDING THE STARTLINE
startLine = which(cars$LAP_DISTANCE == min(cars$LAP_DISTANCE))

startLineCoord = c(cars$WORLDPOSX[startLine],cars$WORLDPOSY[startLine])

#distanceFrom(turns$APEX_X1, cars$WORLDPOSX[startLine], turns$APEX_Y1, cars$WORLDPOSY[startLine])

#PREPARING DATA TO BE FILTERED (Finding distances to corners 1 and 2)
left$c1dist <- sqrt((left$WORLDPOSX - turns$APEX_X1[1])^2+(left$WORLDPOSY - turns$APEX_Y1[1])^2)
right$c1dist <- sqrt((right$WORLDPOSX - turns$APEX_X1[1])^2+(right$WORLDPOSY - turns$APEX_Y1[1])^2)
left$c2dist <- sqrt((left$WORLDPOSX - turns$APEX_X1[2])^2+(left$WORLDPOSY - turns$APEX_Y1[2])^2)
right$c2dist <- sqrt((right$WORLDPOSX - turns$APEX_X1[2])^2+(right$WORLDPOSY - turns$APEX_Y1[2])^2)
slDistToApexOne <- sqrt((cars$WORLDPOSX[startLine] - turns$APEX_X1[1])^2 + (cars$WORLDPOSY[startLine] - turns$APEX_Y1[1])^2)
halfwayDist2and3 = sqrt((turns$APEX_X1[2] - turns$APEX_X1[3])^2+(turns$APEX_X1[2] - turns$APEX_Y1[3])^2)/2


#CREATE FLAG FOR WHETHER WE ARE INTERESTED IN THE POINT (Points near turns 1 and 2)
left$closeToOne <- isCloserThan2(left$c1dist, rep(slDistToApexOne, length(left$c1dist)))
left$closeToTwo <- isCloserThan2(left$c2dist, rep(halfwayDist2and3, length(left$c2dist)))
left$inRange <- (left$closeToOne | left$closeToTwo)


#leftoutrange = left[which(left$inRange == FALSE),]
#leftinrange = left[which(left$inRange == TRUE),]

#rm(leftinrange,leftoutrange)

# plot(leftoutrange$WORLDPOSX, leftoutrange$WORLDPOSY, cex = 0.5, asp = 1)
# points(leftinrange$WORLDPOSX, leftinrange$WORLDPOSY, col = "green", cex = 0.5)
# #text(113,467,"START")
# text(445,191,"1",cex = 2)
# text(439,90,"2",cex = 2)
# text(797,-393,"3",cex = 2)

#rm(cars, left, right, turns, leftinrange, leftoutrange)

#FILTER DATA TO ONLY TURNS 1 AND 2
filtLeft = left[which(left$inRange == TRUE),]

#PLOT FOR SANTIY CHECK OF POINTS
plot(filtLeft$WORLDPOSX, filtLeft$WORLDPOSY)

#CREATE FLAG FOR WHETHER WE ARE INTERESTED IN THE POINT (Points near turns 1 and 2)
right$closeToOne <- isCloserThan2(right$c1dist, rep(slDistToApexOne, length(right$c1dist)))
right$closeToTwo <- isCloserThan2(right$c2dist, rep(halfwayDist2and3, length(right$c2dist)))
right$inRange <- (right$closeToOne | right$closeToTwo) & right$WORLDPOSX > 50

#FILTER DATA TO ONLY TURNS 1 AND 2
filtRight = right[which(right$inRange == TRUE),]

#ORDERING THE POINTS OF THE TRACK
filtLeft["distfromrandpoint"] = distanceFrom(filtLeft$WORLDPOSX, rep(400, length(filtLeft$WORLDPOSX)),filtLeft$WORLDPOSY, rep(900, length(filtLeft$WORLDPOSY)))
filtLeft = filtLeft[order(filtLeft$distfromrandpoint),]

filtRight["distfromrandpoint"] = distanceFrom(filtRight$WORLDPOSX, rep(400, length(filtRight$WORLDPOSX)),filtRight$WORLDPOSY, rep(900, length(filtRight$WORLDPOSY)))
filtRight = filtRight[order(filtRight$distfromrandpoint),]

# #PLOT FOR SANTIY CHECK OF POINTS
# plot(filtRight$WORLDPOSX, filtRight$WORLDPOSY, cex = .1, asp = 1, col = "blue",
#      xlab = "X COORDINATE", ylab = "Y COORDINATE",
#      main = "Full View of Filtered Track Nodes")
# points(filtLeft$WORLDPOSX, filtLeft$WORLDPOSY, cex = .1, col = "red")
# 
# #PLOT FOR SANTIY CHECK OF POINTS
# plot(filtRight$WORLDPOSX, filtRight$WORLDPOSY, cex = .1, asp = 1, col = "blue",
#      xlim = c(100,140), ylim = c(430,470),
#      xlab = "X COORDINATE", ylab = "Y COORDINATE",
#      main = "Nodes in Track Data after filtering")
# points(filtLeft$WORLDPOSX, filtLeft$WORLDPOSY, cex = .1, col = "red")

#############################################################
#FINDING THE POINTS IN THE LEFT TRACK THAT ARE NOT IN A LINE#
#############################################################


# testLeft = filtLeft[1:100,]
# testLeft = testLeft[-c(40,37,35,33,31,28,26,24,22,20,18,15,13,10,8,5,3),]
# 
# plot(testRight$WORLDPOSX[1:50],testRight$WORLDPOSY[1:50],asp = 1)
# 
# for (i in 1:length(testLeft$REFTYPE)) {
#   
#   text(testLeft$WORLDPOSX[i]-5,testLeft$WORLDPOSY[i], i, cex = 0.5)
#   
# }

filtLeft = filtLeft[-c(40,37,35,33,31,28,26,24,22,20,18,15,13,10,8,5,3),]

# #FINDING THE POINTS IN THE RIGHT TRACK THAT ARE NOT IN A LINE
# testRight = filtRight[1:100,]
# testRight = testRight[-c(3,5,6,7,9,10,11,12,14,15,16,18,19,20,22,23,24,25,26,
#                          28,29,30,31,33,34,35,36,38,39,40,41,42,43,44,45,46,47,48,
#                          49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71),]
# 
# plot(testRight$WORLDPOSX[1:90],testRight$WORLDPOSY[1:90],asp = 1)
# 
# for (i in 1:length(testRight$REFTYPE)) {
#   
#   text(testRight$WORLDPOSX[i],testRight$WORLDPOSY[i]-5, i, cex = 0.5)
#   
# }

filtRight = filtRight[-c(3,5,6,7,9,10,11,12,14,15,16,18,19,20,22,23,24,25,26,
                         28,29,30,31,33,34,35,36,38,39,40,41,42,43,44,45,46,47,48,
                         49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71),]


# Construct left side of track

x1 = filtLeft$WORLDPOSX
x2 = append(x1, filtLeft$WORLDPOSX[length(filtLeft$WORLDPOSX)])[-1]

y1 = filtLeft$WORLDPOSY
y2 = append(y1, filtLeft$WORLDPOSY[length(filtLeft$WORLDPOSY)])[-1]

distToNext = append(c(0), distanceFrom(x1,x2,y1,y2))[-length(append(c(0), distanceFrom(x1,x2,y1,y2)))]

filtLeft["distToNext"] = distToNext

cumsum = c()

for(i in 1:length(filtLeft$WORLDPOSX)) {
  
  cumsum = append(cumsum, sum(filtLeft$distToNext[1:i]) - sum(distToNext[1:5])) 
    
}

filtLeft["LAP_DISTANCE_TOTAL"] = cumsum
filtLeft["LAP_DISTANCE_PERC"] = filtLeft$LAP_DISTANCE_TOTAL / filtLeft$LAP_DISTANCE_TOTAL[length(filtLeft$LAP_DISTANCE_TOTAL) - 4]

# Construct right side of track

x1 = filtRight$WORLDPOSX
x2 = append(x1, filtRight$WORLDPOSX[length(filtRight$WORLDPOSX)])[-1]

y1 = filtRight$WORLDPOSY
y2 = append(y1, filtRight$WORLDPOSY[length(filtRight$WORLDPOSY)])[-1]

distToNext = append(c(0), distanceFrom(x1,x2,y1,y2))[-length(append(c(0), distanceFrom(x1,x2,y1,y2)))]

filtRight["distToNext"] = distToNext

cumsum = c()

for(i in 1:length(filtRight$WORLDPOSX)) {
  
  cumsum = append(cumsum, sum(filtRight$distToNext[1:i]) - sum(distToNext[1:5])) 
  
}


filtRight["LAP_DISTANCE_TOTAL"] = cumsum
filtRight["LAP_DISTANCE_PERC"] = filtRight$LAP_DISTANCE_TOTAL / filtRight$LAP_DISTANCE_TOTAL[length(filtRight$LAP_DISTANCE_TOTAL) - 4]

rm(cumsum, distToNext, startLineCoord, x1, x2, y1, y2, i, halfwayDist2and3, slDistToApexOne, startLine)

rm(left,right)

write_csv(filtLeft, "filtLeft.csv")
write_csv(filtRight, "filtRight.csv")


print("RUN SUCCESSFULLY: constructingTrackSidesv1.1.R")
