# 2022 variables


dist2d <- function(a,b,c) {
  v1 <- b - c
  v2 <- a - b
  m <- cbind(v1,v2)
  d <- abs(det(m))/sqrt(sum(v1*v1))
} 


# Using same start line and finish line, filter the data:

x_start = c(120.4416, 110.9773)
y_start = c(463.2061, 453.3787)

leftStartCoord = c(120.4416, 463.2061)
rightStartCoord = c(110.9773, 453.3787)

x_finish = c(641.6497,  634.1591)
y_finish = c(-221.5224, -226.8029)

leftFinishCoord = c(641.6497, -221.5224)
rightFinishCoord = c(634.1591, -226.8029)

start_line_intercept = summary(lm(y_start ~ x_start))$coefficients[1,1]
start_line_gradient = summary(lm(y_start ~ x_start))$coefficients[2,1]

finish_line_intercept = summary(lm(y_finish ~ x_finish))$coefficients[1,1]
finish_line_gradient = summary(lm(y_finish ~ x_finish))$coefficients[2,1]

cars$AboveStart <- (cars$WORLDPOSY > start_line_intercept + start_line_gradient * cars$WORLDPOSX)
cars$AboveFinish <- (cars$WORLDPOSY > finish_line_intercept + finish_line_gradient * cars$WORLDPOSX)

# check if any of the frames already lie on the start and finish line

cars$LiesOnStartLine <- (cars$WORLDPOSY == start_line_intercept + start_line_gradient * cars$WORLDPOSX)
cars[which(cars$LiesOnStartLine == TRUE),]


cars$LiesOnFinish <- (cars$WORLDPOSY == finish_line_intercept + finish_line_gradient * cars$WORLDPOSX)
cars[which(cars$LiesOnFinishLine == TRUE),]

cars$DistanceFromStartLeft <- sqrt((cars$WORLDPOSX - x_start[1])^2+(cars$WORLDPOSY - y_start[1])^2)
cars$DistanceFromStartRight <- sqrt((cars$WORLDPOSX - x_start[2])^2+(cars$WORLDPOSY - y_start[2])^2)

cars$DistanceFromFinishLeft <- sqrt((cars$WORLDPOSX - x_finish[1])^2+(cars$WORLDPOSY - y_finish[1])^2)
cars$DistanceFromFinishRight <- sqrt((cars$WORLDPOSX - x_finish[2])^2+(cars$WORLDPOSY - y_finish[2])^2)


# Obtain the first 20 frames for each lap

starting_frames = cars %>% group_by(SESSION_IDENTIFIER, LAP_NUM) %>% slice_min(LAP_DISTANCE, n=20)
# add exclusion criteria based on distance

# Check if there is at least one frame above our designated start_line for every lap

problem_sessionIds = c()
problem_lapNums = c()

for (session_id in unique(starting_frames$SESSION_IDENTIFIER)) {
  for (lap_num in unique(starting_frames[which(starting_frames$SESSION_IDENTIFIER == session_id),]$LAP_NUM)) {
    current_frame = starting_frames[which(starting_frames$SESSION_IDENTIFIER == session_id & starting_frames$LAP_NUM == lap_num),]
   
    # for these frames, check the distance of the first point from the start line 
    
    if (is.element(TRUE, current_frame$AboveStart) == FALSE) {
      print("None Above Start!")
      problem_sessionIds = c(problem_sessionIds, session_id)
      problem_lapNums = c(problem_lapNums, lap_num)
    }
  }
}

# First problem lap has no frames: remove

cars = cars[!(cars$SESSION_IDENTIFIER == problem_sessionIds[1] &
                cars$LAP_NUM == problem_lapNums[1]),]
filtCars = filtCars[!(filtCars$SESSION_IDENTIFIER == problem_sessionIds[1] &
                        filtCars$LAP_NUM == problem_lapNums[1]),]

# Other three problem laps have no frames above start, but all other frames are okay
  # process: calculate distance from start line to the first frame
  # use the speed variable at first frame

problem_frame_2 = starting_frames[which(starting_frames$SESSION_IDENTIFIER == problem_sessionIds[2] & starting_frames$LAP_NUM == problem_lapNums[2]),]
problem_frame_3 = starting_frames[which(starting_frames$SESSION_IDENTIFIER == problem_sessionIds[3] & starting_frames$LAP_NUM == problem_lapNums[3]),]
problem_frame_4 = starting_frames[which(starting_frames$SESSION_IDENTIFIER == problem_sessionIds[4] & starting_frames$LAP_NUM == problem_lapNums[4]),]


filtCars$NORMALISED_LAP_TIME_MS = 0

for (session_id in unique(starting_frames$SESSION_IDENTIFIER)) {
  for (lap_num in unique(starting_frames[which(starting_frames$SESSION_IDENTIFIER == session_id),]$LAP_NUM)) {
    current_frame = starting_frames[which(starting_frames$SESSION_IDENTIFIER == session_id & starting_frames$LAP_NUM == lap_num),]
    
    if (identical(current_frame, problem_frame_2) == TRUE 
        || identical(current_frame, problem_frame_3) == TRUE
        || identical(current_frame, problem_frame_4) == TRUE
        ) {
      first_point = c(head(current_frame, 1)$WORLDPOSX, head(current_frame, 1)$WORLDPOSY)
      distance_from_start_line_to_first_point = dist2d(first_point, leftStartCoord, rightStartCoord)
      time_to_first_point = distance_from_start_line_to_first_point / head(current_frame, 1)$SPEED_KPH
      official_diff_current_time_ms = head(current_frame, 1)$CURRENT_LAP_TIME_MS - time_to_first_point
      filtCars[which(filtCars$SESSION_IDENTIFIER == session_id & filtCars$LAP_NUM == lap_num),]$NORMALISED_LAP_TIME_MS = filtCars[which(filtCars$SESSION_IDENTIFIER == session_id & filtCars$LAP_NUM == lap_num),]$CURRENT_LAP_TIME_MS - official_diff_current_time_ms
      next
    }
    
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


finishing_frames = 
  cars %>% group_by(SESSION_IDENTIFIER, LAP_NUM) %>% slice_min(DistanceFromFinishLeft, n=20)

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


lapList = 
  
  filtCars %>%
  group_by(SESSION_IDENTIFIER, LAP_NUM) %>%
  slice(which.max(NORMALISED_LAP_TIME_MS))

lapList = arrange(lapList, NORMALISED_LAP_TIME_MS)

#EXTRACT DATA FOR LAP WITH THE NTH FASTEST TIME FOR RANGE
findLap <- function(rank) {
  
  sel_sessionid = lapList$SESSION_IDENTIFIER[rank]
  sel_lapnumid = lapList$LAP_NUM[rank]
  
  return(filtCars[filtCars$SESSION_IDENTIFIER == sel_sessionid & filtCars$LAP_NUM == sel_lapnumid,])
  
}

filtCars$THROTTLE_CHANGE = 0
for (i in 1:nrow(lapList)) {
  current_lap = findLap(i)
  filtCars[which(filtCars$SESSION_IDENTIFIER == unique(current_lap$SESSION_IDENTIFIER) & filtCars$LAP_NUM == unique(current_lap$LAP_NUM)),] = filtCars[which(filtCars$SESSION_IDENTIFIER == unique(current_lap$SESSION_IDENTIFIER) & filtCars$LAP_NUM == unique(current_lap$LAP_NUM)),] %>%  mutate(THROTTLE_CHANGE = THROTTLE - lag(THROTTLE, ))
}
filtCars["THROTTLE_CHANGE"][is.na(filtCars["THROTTLE_CHANGE"])] <- 0

filtCars$INCREASING_THROTTLE = (filtCars$THROTTLE_CHANGE > 0)
filtCars$DECREASING_THROTTLE = (filtCars$THROTTLE_CHANGE < 0)

# Visualise the throttle patterns on the track (decreasing, increasing, same)

for (i in 1:nrow(lapList)) {
  lap = findLap(i)
  plot(filtLeft$WORLDPOSX, filtLeft$WORLDPOSY, cex = 0.1)
  points(filtRight$WORLDPOSX, filtRight$WORLDPOSY, cex = 0.1)
  lines(x = x_start,y=y_start,col = "red", lwd=5)
  lines(x = x_finish,y=y_finish,col = "red", lwd=5)
  for (j in 2:nrow(lap)) {
    frame = lap[j,]
    if (frame$INCREASING_THROTTLE == TRUE) {
      points(frame$WORLDPOSX, frame$WORLDPOSY,  col = ('green'), cex = 0.2)
    } else if (frame$DECREASING_THROTTLE == TRUE) {
      points(frame$WORLDPOSX, frame$WORLDPOSY,  col = ('red'), cex = 0.2)
    } else {
      points(frame$WORLDPOSX, frame$WORLDPOSY,  col = ('yellow'), cex = 0.2)
    }
  }
  title(main = paste("Lap", i))
}