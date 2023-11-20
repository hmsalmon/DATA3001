# Make sure you run the files in the following order:
  # constructingTrackSides.R
  # projectDataCleaningv1.2.R
  # findStart&Finish.R

# Note that I haven't extrapolated any other variables to the start and finish line
  # only the lap time
  

# Finds distance between point a and line segment defined by points b and c
dist2d <- function(a,b,c) {
  v1 <- b - c
  v2 <- a - b
  m <- cbind(v1,v2)
  d <- abs(det(m))/sqrt(sum(v1*v1))
} 

# Finds point (x,y) on a line segment defined by Coord1 and Coord2 perpendicular 
# to another point Point

perpPoint <- function(Coord1, Coord2, Point) {
  k = ((Coord2[2]-Coord1[2]) * (Point[1]-Coord1[1]) - (Coord2[1]-Coord1[1]) * (Point[2]-Coord1[2])) / ((Coord2[2]-Coord1[2])^2 + (Coord2[1]-Coord1[1])^2)
  x = Point[1] - k * (Coord2[2]-Coord1[2])
  y = Point[2] + k * (Coord2[1]-Coord1[1])
  perpPoint <- c(x, y)
}


# find starting line coordinates 

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


# find finish line coordinates

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


# plot start and finish line

plot(filtLeft$WORLDPOSX, filtLeft$WORLDPOSY, cex = 0.1)
points(filtRight$WORLDPOSX, filtRight$WORLDPOSY, cex = 0.1)
lines(x = x_start,y=y_start,col = "red", lwd=5)
lines(x = x_finish,y=y_finish,col = "red", lwd=5)
  # pick a normal straight line across from the left side


# obtain equations of start and finish line

start_line_intercept = summary(lm(y_start ~ x_start))$coefficients[1,1]
start_line_gradient = summary(lm(y_start ~ x_start))$coefficients[2,1]

finish_line_intercept = summary(lm(y_finish ~ x_finish))$coefficients[1,1]
finish_line_gradient = summary(lm(y_finish ~ x_finish))$coefficients[2,1]

# check if any of the frames already lie on the start and finish line

cleanCars$LiesOnStartLine <- (cleanCars$WORLDPOSY == start_line_intercept + start_line_gradient * cleanCars$WORLDPOSX)
cleanCars[which(cleanCars$LiesOnStartLine == TRUE),]


cleanCars$LiesOnFinish <- (cleanCars$WORLDPOSY == finish_line_intercept + finish_line_gradient * cleanCars$WORLDPOSX)
cleanCars[which(cleanCars$LiesOnFinishLine == TRUE),]
  
  # none of them do
  

# identify the frames for each lap closest to the start and finish line

cleanCars$DistanceFromStartLeft <- sqrt((cleanCars$WORLDPOSX - x_start[1])^2+(cleanCars$WORLDPOSY - y_start[1])^2)
cleanCars$DistanceFromStartRight <- sqrt((cleanCars$WORLDPOSX - x_start[2])^2+(cleanCars$WORLDPOSY - y_start[2])^2)

cleanCars$DistanceFromFinishLeft <- sqrt((cleanCars$WORLDPOSX - x_finish[1])^2+(cleanCars$WORLDPOSY - y_finish[1])^2)
cleanCars$DistanceFromFinishRight <- sqrt((cleanCars$WORLDPOSX - x_finish[2])^2+(cleanCars$WORLDPOSY - y_finish[2])^2)


# Check which frames are above or below the start and finish line

cleanCars$AboveStart <- (cleanCars$WORLDPOSY > start_line_intercept + start_line_gradient * cleanCars$WORLDPOSX)
cleanCars$AboveFinish <- (cleanCars$WORLDPOSY > finish_line_intercept + finish_line_gradient * cleanCars$WORLDPOSX)

# Obtain the first 20 frames for each lap

starting_frames = cleanCars %>% group_by(SESSION_IDENTIFIER, LAP_NUM) %>% slice_min(LAP_DISTANCE, n=20)
  # add exclusion criteria based on distance

# Check if there is at least one frame above our designated start_line for every lap

for (session_id in unique(starting_frames$SESSION_IDENTIFIER)) {
  for (lap_num in unique(starting_frames[which(starting_frames$SESSION_IDENTIFIER == session_id),]$LAP_NUM)) {
    current_frame = starting_frames[which(starting_frames$SESSION_IDENTIFIER == session_id & starting_frames$LAP_NUM == lap_num),]
    if (is.element(TRUE, current_frame$AboveStart) == FALSE) {
      print("None Above Start!")
      problem_frame <- current_frame
    }
  }
}

  # we have found that lap_num = 6 for session_identifier = 4.49e18 does not have a starting_frame above the line

# plot the problem_frame
# plot(cleanCars[which(cleanCars$SESSION_IDENTIFIER == problem_frame$SESSION_IDENTIFIER & cleanCars$LAP_NUM == 6),]$WORLDPOSX, cleanCars[which(cleanCars$SESSION_IDENTIFIER == problem_frame$SESSION_IDENTIFIER & cleanCars$LAP_NUM == 6),]$WORLDPOSY)

  # it has missing data for large chunks - should we disregard?



# Considering this is the only problem frame, let's continue with the starting_frame being the 5th frame

# Remove the problem lap

# filtCars <- subset(filtCars, filtCars$SESSION_IDENTIFIER != problem_frame$SESSION_IDENTIFIER & filtCars$LAP_NUM != problem_frame$LAP_NUM)

# Now average last frame above and first frame below the start line to obtain 
# a normalised lap time

filtCars$NORMALISED_LAP_TIME_MS = 0

for (session_id in unique(starting_frames$SESSION_IDENTIFIER)) {
  for (lap_num in unique(starting_frames[which(starting_frames$SESSION_IDENTIFIER == session_id),]$LAP_NUM)) {
    current_frame = starting_frames[which(starting_frames$SESSION_IDENTIFIER == session_id & starting_frames$LAP_NUM == lap_num),]
    
    # Get last frame above and first frame below the start line
    last_frame_above = tail(current_frame[which(current_frame$AboveStart == TRUE),], 1)
    first_frame_below = head(current_frame[which(current_frame$AboveStart == FALSE),], 1)
    
    # Calculate the distance between these two points and the time between them to work out the cars
    # average speed over these two frames
    distance_travelled_m = first_frame_below$LAP_DISTANCE - last_frame_above$LAP_DISTANCE
    time_taken_ms = first_frame_below$CURRENT_LAP_TIME_MS - last_frame_above$CURRENT_LAP_TIME_MS
    
    average_speed_m_per_ms = distance_travelled_m / time_taken_ms
    
    
    # Calculate the distance of the first frame below the start line from the starting line 
    # and the time taken to travel this distance based off of the average speed
    first_point = c(first_frame_below$WORLDPOSX[1], first_frame_below$WORLDPOSY[1])

    distance_from_start_line_to_first_point = dist2d(first_point, leftStartCoord, rightStartCoord)
    time_to_first_point = distance_from_start_line_to_first_point / average_speed_m_per_ms  
    
    # Subtract from the current lap time for every frame in that lap by the time it took to get to 
    # our designated starting line
    official_diff_current_time_ms = first_frame_below$CURRENT_LAP_TIME_MS - time_to_first_point
    
    filtCars[which(filtCars$SESSION_IDENTIFIER == session_id & filtCars$LAP_NUM == lap_num),]$NORMALISED_LAP_TIME_MS = filtCars[which(filtCars$SESSION_IDENTIFIER == session_id & filtCars$LAP_NUM == lap_num),]$CURRENT_LAP_TIME_MS - official_diff_current_time_ms 
  }
}


# Obtain the 20 closest frames to the finish line for each lap

filtCars1 = cleanCars[which(cleanCars$SECTOR == 0),]

finishing_frames = 
  filtCars1 %>% group_by(SESSION_IDENTIFIER, LAP_NUM) %>% slice_min(DistanceFromFinishLeft, n=20)

finishing_frames = finishing_frames[order(finishing_frames$SESSION_IDENTIFIER, finishing_frames$LAP_NUM, finishing_frames$LAP_DISTANCE),]


# Check if there is at least one frame above and below our designated finish_line for every lap

for (session_id in unique(finishing_frames$SESSION_IDENTIFIER)) {
  for (lap_num in unique(finishing_frames[which(finishing_frames$SESSION_IDENTIFIER == session_id),]$LAP_NUM)) {
    current_frame = finishing_frames[which(finishing_frames$SESSION_IDENTIFIER == session_id & finishing_frames$LAP_NUM == lap_num),]
    if (is.element(TRUE, current_frame$AboveFinish) == FALSE) {
      print("None Above Finish!")
      problem_frame <- current_frame
    }
    
    if (is.element(FALSE, current_frame$AboveFinish) == FALSE) {
      print("None Below Finish!")
      problem_frame <- current_frame
    }
  }
}


# Now add extra time to the final frame before the finish line to obtain 
# a normalised lap time

for (session_id in unique(finishing_frames$SESSION_IDENTIFIER)) {
  for (lap_num in unique(finishing_frames[which(finishing_frames$SESSION_IDENTIFIER == session_id),]$LAP_NUM)) {
    current_frame = finishing_frames[which(finishing_frames$SESSION_IDENTIFIER == session_id & finishing_frames$LAP_NUM == lap_num),]
    
    # Obtain the last frame above and first frame below the finish line for the given lap
    last_frame_above = tail(current_frame[which(current_frame$AboveFinish == TRUE),], 1)
    first_frame_below = head(current_frame[which(current_frame$AboveFinish == FALSE),], 1)
    
    # Calculate the distance travelled and time taken between these frames to obtain average speed
    distance_travelled_m = first_frame_below$LAP_DISTANCE - last_frame_above$LAP_DISTANCE
    time_taken_ms = first_frame_below$CURRENT_LAP_TIME_MS - last_frame_above$CURRENT_LAP_TIME_MS

    average_speed_m_per_ms = distance_travelled_m / time_taken_ms
    
    # Calculate the distance from the last frame before the finish line to the finish line
    # and the time taken based off of the average speed
    final_point = c(last_frame_above$WORLDPOSX[1], last_frame_above$WORLDPOSY[1])
    
    distance_from_final_point_to_finish = dist2d(final_point, leftFinishCoord, rightFinishCoord)
    time_to_finish = distance_from_final_point_to_finish / average_speed_m_per_ms
    
    # Add this extra bit of time to reach our designated finish line to the last frame 
    # before our finsh line for every lap
    
    filtCars[which(filtCars$SESSION_IDENTIFIER == session_id & filtCars$LAP_NUM == lap_num & filtCars$FRAME == last_frame_above$FRAME),]$NORMALISED_LAP_TIME_MS = filtCars[which(filtCars$SESSION_IDENTIFIER == session_id & filtCars$LAP_NUM == lap_num & filtCars$FRAME == last_frame_above$FRAME),]$NORMALISED_LAP_TIME_MS + time_to_finish 
  }
}

# Filter for the frames within our start and finish line
frames_within_lap = cleanCars[which(cleanCars$AboveStart == FALSE & cleanCars$AboveFinish == TRUE & cleanCars$SECTOR == 0),]


# Sanity check to see if frames are in the right region

plot(left$WORLDPOSX, left$WORLDPOSY, cex = 0.05)
points(right$WORLDPOSX, right$WORLDPOSY, cex = 0.05)
lines(x = x_start,y=y_start,col = "red", lwd=5)
lines(x = x_finish,y=y_finish,col = "red", lwd=5)
points(frames_within_lap$WORLDPOSX, frames_within_lap$WORLDPOSY, col = 'green', cex = 0.01)
