
filtCarsStored = filtCars


# Finds distance between point a and line segment defined by points b and c
dist2d <- function(a,b,c) {
  v1 <- b - c
  v2 <- a - b
  m <- cbind(v1,v2)
  d <- abs(det(m))/sqrt(sum(v1*v1))
} 

firstFrame = 
  
  filtCars %>%
  group_by(SESSION_IDENTIFIER, LAP_NUM) %>%
  slice(which.min(LAP_DISTANCE_PERC)) 

firstAfterStartlaps = which(firstFrame$LAP_DISTANCE_PERC > 0)
numFirstAfterStartLaps = length(firstAfterStartlaps)

firstFrame = arrange(firstFrame, desc(LAP_DISTANCE_PERC))

badsess1 = firstFrame$SESSION_IDENTIFIER[1]
badsess2 = firstFrame$SESSION_IDENTIFIER[2]
badlap1 = firstFrame$LAP_NUM[1]
badlap2 = firstFrame$LAP_NUM[2]

plotlap(filtCars[filtCars$SESSION_IDENTIFIER == badsess1 & filtCars$LAP_NUM == badlap1,],"bad1")
plotlap(filtCars[filtCars$SESSION_IDENTIFIER == badsess2 & filtCars$LAP_NUM == badlap2,],"bad2")


filtCars = filtCars[-which(((filtCars$SESSION_IDENTIFIER == badsess1) & (filtCars$LAP_NUM == badlap1))
                          | ((filtCars$SESSION_IDENTIFIER == badsess2) & (filtCars$LAP_NUM == badlap2))),]



lastFrame = 
  
  filtCars %>%
  group_by(SESSION_IDENTIFIER, LAP_NUM) %>%
  slice(which.max(LAP_DISTANCE_PERC))

lastBeforeFinishlaps = which(lastFrame$LAP_DISTANCE_PERC < 1)
numLastBeforeFinishlaps = length(lastBeforeFinishlaps)

# Now average last frame above and first frame below the start line to obtain 
# a normalised lap time

starting_frames =
  filtCars %>%
  group_by(SESSION_IDENTIFIER, LAP_NUM) %>%
  slice_min(LAP_DISTANCE, n=20)


filtCars$NORMALISED_LAP_TIME_MS = 0

for (session_id in unique(starting_frames$SESSION_IDENTIFIER)) {
  for (lap_num in unique(starting_frames[which(starting_frames$SESSION_IDENTIFIER == session_id),]$LAP_NUM)) {
    current_frame = starting_frames[which(starting_frames$SESSION_IDENTIFIER == session_id & starting_frames$LAP_NUM == lap_num),]
    
    # Get last frame above and first frame below the start line
    last_frame_above = tail(current_frame[which(current_frame$AboveStart2 == TRUE),], 1)
    first_frame_below = head(current_frame[which(current_frame$AboveStart2 == FALSE),], 1)
    
    # Calculate the distance between these two points and the time between them to work out the cars
    # average speed over these two frames
    distance_travelled_m = first_frame_below$LAP_DISTANCE_TOTAL - last_frame_above$LAP_DISTANCE_TOTAL
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

filtCarsStored2 = filtCars

finishing_frames = 
  filtCars %>% group_by(SESSION_IDENTIFIER, LAP_NUM) %>% slice_max(LAP_DISTANCE_PERC, n=20)

finishing_frames = finishing_frames[order(finishing_frames$SESSION_IDENTIFIER, finishing_frames$LAP_NUM, finishing_frames$LAP_DISTANCE_PERC),]

# Now add extra time to the final frame before the finish line to obtain 
# a normalised lap time

for (session_id in unique(finishing_frames$SESSION_IDENTIFIER)) {
  for (lap_num in unique(finishing_frames[which(finishing_frames$SESSION_IDENTIFIER == session_id),]$LAP_NUM)) {
    current_frame = finishing_frames[which(finishing_frames$SESSION_IDENTIFIER == session_id & finishing_frames$LAP_NUM == lap_num),]
    
    # Obtain the last frame above and first frame below the finish line for the given lap
    last_frame_above = tail(current_frame[which(current_frame$AboveFinish2 == TRUE),], 1)
    first_frame_below = head(current_frame[which(current_frame$AboveFinish2 == FALSE),], 1)
    
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
#frames_within_lap = filtCars[which(filtCars$AboveStart == FALSE & filtCars$AboveFinish == TRUE & filtCars$SECTOR == 0),]
frames_within_lap = filtCars[which(filtCars$LAP_DISTANCE_PERC >=0 & filtCars$LAP_DISTANCE_PERC <=1),]

# Drop unnecessary columns

frames_within_lap = subset(frames_within_lap, select = -c(AboveStart,
                                                          AboveFinish,
                                                          AboveStart2,
                                                          AboveFinish2))

filtCars = subset(filtCars, select = -c(AboveStart,
                                        AboveFinish,
                                        AboveStart2,
                                        AboveFinish2))


filtCars2 = frames_within_lap

write_csv(filtCars,"filtCars_inRegion.csv")
write_csv(filtCars2,"filtCars_inTimedRange.csv")


rm(current_frame, finishing_frames, first_frame_below, frames_within_lap, last_frame_above, starting_frames,
   firstFrame, lastFrame)

#rm(filtCarsStored, filtCarsStored2)

rm(average_speed_m_per_ms, distance_from_final_point_to_finish, distance_from_start_line_to_first_point,
   distance_travelled_m, final_point, first_point, lap_num,
   leftFinishCoord, leftStartCoord, official_diff_current_time_ms, rightFinishCoord, rightStartCoord,
   session_id, time_taken_ms, time_to_finish, time_to_first_point, firstAfterStartlaps, lastBeforeFinishlaps,
   numFirstAfterStartLaps, numLastBeforeFinishlaps)

print("RUN SUCCESSFULLY: findStart&Finishv2.0.R")
