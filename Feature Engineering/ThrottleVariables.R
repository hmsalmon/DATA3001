# MAKE SURE YOU'VE LOADED THE FRAMES_WITHIN_LAP DF FROM findStart&FinishRevamped.R

# Construct a list of one frame per lap from the frames_within_lap dataframe

filtered_lap_list <- frames_within_lap %>% group_by(SESSION_IDENTIFIER, LAP_NUM) %>% slice(which.max(NORMALISED_LAP_TIME_MS))

filtered_lap_list = arrange(filtered_lap_list, NORMALISED_LAP_TIME_MS)

findLap <- function(rank) {
  
  sel_sessionid = filtered_lap_list$SESSION_IDENTIFIER[rank]
  sel_lapnumid = filtered_lap_list$LAP_NUM[rank]
  
  return(frames_within_lap[frames_within_lap$SESSION_IDENTIFIER == sel_sessionid & frames_within_lap$LAP_NUM == sel_lapnumid,])
  
}

lap1 <- findLap(1)
lap2 <- findLap(2)
lap100 <- findLap(100)
lap200 <- findLap(200)
lap300 <- findLap(300)
lap496 <- findLap(496)
lap497 <- findLap(497)

# Plot fastest lap racing line
plot(lap1$WORLDPOSX, lap1$WORLDPOSY, col = 'green', cex = 0.1)
points(left$WORLDPOSX, left$WORLDPOSY, cex = 0.05)
points(right$WORLDPOSX, right$WORLDPOSY, cex = 0.05)
lines(x = x_start,y=y_start,col = "red", lwd=5)
lines(x = x_finish,y=y_finish,col = "red", lwd=5)

# Plot fastest lap throttle/brake comparison

plot(lap1$LAP_DISTANCE, lap1$THROTTLE, col = 'red', type = "l")
lines(lap1$LAP_DISTANCE, lap1$BRAKE)


# Where is Throttle Greater than 0
frames_within_lap$OnThrottle <- frames_within_lap$THROTTLE != 0

# First point of the throttle

# Divide track into pre turn 1, between turn 1 and turn 2, post turn 2

# find equation of line representing beginning of turn 1

turn1_xs = c(turns$CORNER_X1[1], turns$CORNER_X2[1])
turn1_ys = c(turns$CORNER_Y1[1], turns$CORNER_Y2[1])
turn1_intercept = summary(lm(turn1_ys ~ turn1_xs))$coefficients[1,1]
turn1_gradient = summary(lm(turn1_ys ~ turn1_xs))$coefficients[2,1]

# find equation of line representing beginning of turn 2

turn2_xs = c(turns$CORNER_X1[2], turns$CORNER_X2[2])
turn2_ys = c(turns$CORNER_Y1[2], turns$CORNER_Y2[2])
turn2_intercept = summary(lm(turn2_ys ~ turn2_xs))$coefficients[1,1]
turn2_gradient = summary(lm(turn2_ys ~ turn2_xs))$coefficients[2,1]

# Divide track into pre turn 1, between turn 1 and turn 2, post turn 2

frames_within_lap$PreTurn1 <- (frames_within_lap$WORLDPOSY > turn1_intercept + turn1_gradient * frames_within_lap$WORLDPOSX)
frames_within_lap$BtwnTurn1And2 <- ((frames_within_lap$WORLDPOSY < turn1_intercept + turn1_gradient * frames_within_lap$WORLDPOSX) & frames_within_lap$WORLDPOSY > turn2_intercept + turn2_gradient * frames_within_lap$WORLDPOSX)
frames_within_lap$PostTurn2 <- (frames_within_lap$WORLDPOSY < turn2_intercept + turn2_gradient * frames_within_lap$WORLDPOSX)

frames_pre_turn1 <- frames_within_lap[which(frames_within_lap$PreTurn1 == TRUE), ]
frames_btwn_turn1and2 <- frames_within_lap[which(frames_within_lap$BtwnTurn1And2 == TRUE), ]
frames_post_turn2 <- frames_within_lap[which(frames_within_lap$PostTurn2 == TRUE), ]

# Plot points to check if track is divided appropriately

plot(filtLeft$WORLDPOSX, filtLeft$WORLDPOSY, cex = 0.1, xlab = 'WORLDPOSX', ylab = 'WORLDPOSY')
points(filtRight$WORLDPOSX, filtRight$WORLDPOSY, cex = 0.1)
lines(x = x_start,y=y_start,col = "red", lwd=5)
lines(x = x_finish,y=y_finish,col = "red", lwd=5)
segments(turns$CORNER_X1[1], turns$CORNER_Y1[1], turns$CORNER_X2[1], turns$CORNER_Y2[1], lwd = 5)
segments(turns$CORNER_X1[2], turns$CORNER_Y1[2], turns$CORNER_X2[2], turns$CORNER_Y2[2], lwd = 5)
points(frames_pre_turn1$WORLDPOSX, frames_pre_turn1$WORLDPOSY, col = 'blue', cex = 0.01)
points(frames_btwn_turn1and2$WORLDPOSX, frames_btwn_turn1and2$WORLDPOSY, col = 'green', cex = 0.01)
points(frames_post_turn2$WORLDPOSX, frames_post_turn2$WORLDPOSY, col = 'yellow', cex = 0.01)
title(main = 'Track Division Based on Corners')
legend(x="topright", legend = c("Pre Turn 1", "Between Turns", "Post Turn 2"),  fill = c("blue","green", "yellow"))

filtered_lap_list$FIRST_FRAME_OFF_THROTTLE = 0

filtered_lap_list$FIRST_FRAME_BACK_ON_THROTTLE = 0

filtered_lap_list$AVG_THROTTLE_PRE_TURN1 = 0

filtered_lap_list$AVG_THROTTLE_BTWN_TURN1AND2 = 0

filtered_lap_list$AVG_THROTTLE_POST_TURN2 = 0


frames_within_lap$INCREASING_THROTTLE = FALSE
frames_within_lap$SAME_THROTTLE = FALSE
frames_within_lap$DECREASING_THROTTLE = FALSE

# NOTE - this loop takes about 20 minutes to identify increasing/decreasing throttle

for (session_id in unique(frames_within_lap$SESSION_IDENTIFIER)) {
  for (lap_num in unique(frames_within_lap[which(frames_within_lap$SESSION_IDENTIFIER == session_id),]$LAP_NUM)) {
    current_lap = frames_within_lap[which(frames_within_lap$SESSION_IDENTIFIER == session_id & frames_within_lap$LAP_NUM == lap_num),]
    
    
    # Find first frame off the throttle
    first_frame_off_throttle = head(current_lap[which(current_lap$THROTTLE != 1),], 1)$FRAME
    filtered_lap_list[which(filtered_lap_list$SESSION_IDENTIFIER == session_id & filtered_lap_list$LAP_NUM == lap_num),]$FIRST_FRAME_OFF_THROTTLE = first_frame_off_throttle
    
    
    # Find first frame back on the throttle
    
    first_frame_on_throttle = (tail(current_lap[which(current_lap$THROTTLE == 0),], 1)$FRAME + 1)
    if (identical(first_frame_on_throttle, numeric(0))) {
      print(first_frame_on_throttle)
      first_frame_on_throttle = 0
    }
    filtered_lap_list[which(filtered_lap_list$SESSION_IDENTIFIER == session_id & filtered_lap_list$LAP_NUM == lap_num),]$FIRST_FRAME_BACK_ON_THROTTLE = first_frame_on_throttle
    
    # identify increasing/decreasing throttle (make NA for first throttle frame)
    
    current_throttle = head(current_lap, 1)$THROTTLE
    
   for (i in 1:nrow(current_lap[-1,])) {
     frame = current_lap[i,]
     previous_throttle = current_throttle
     current_throttle = frame$THROTTLE
     if (current_throttle > previous_throttle) {
         frames_within_lap[which(frames_within_lap$SESSION_IDENTIFIER == session_id & frames_within_lap$LAP_NUM == lap_num & frames_within_lap$FRAME == frame$FRAME),]$INCREASING_THROTTLE = TRUE
     } else if (current_throttle == previous_throttle) {
       frames_within_lap[which(frames_within_lap$SESSION_IDENTIFIER == session_id & frames_within_lap$LAP_NUM == lap_num & frames_within_lap$FRAME == frame$FRAME),]$SAME_THROTTLE = TRUE
     } else {
       frames_within_lap[which(frames_within_lap$SESSION_IDENTIFIER == session_id & frames_within_lap$LAP_NUM == lap_num & frames_within_lap$FRAME == frame$FRAME),]$DECREASING_THROTTLE = TRUE
     }
   }
    
    # Find average throttle pre turn 1
    preTurn1 = current_lap[which(current_lap$PreTurn1 == TRUE), ]
    filtered_lap_list[which(filtered_lap_list$SESSION_IDENTIFIER == session_id & filtered_lap_list$LAP_NUM == lap_num),]$AVG_THROTTLE_PRE_TURN1 = mean(preTurn1$THROTTLE)
    
    # Find average throttle between turns 1 and 2
    
    btwnTurn1And2 = current_lap[which(current_lap$BtwnTurn1And2 == TRUE), ]
    filtered_lap_list[which(filtered_lap_list$SESSION_IDENTIFIER == session_id & filtered_lap_list$LAP_NUM == lap_num),]$AVG_THROTTLE_BTWN_TURN1AND2 = mean(btwnTurn1And2$THROTTLE)
    
    
    # Find average throttle post turn 2
    
    postTurn2 = current_lap[which(current_lap$PostTurn2 == TRUE), ]
    filtered_lap_list[which(filtered_lap_list$SESSION_IDENTIFIER == session_id & filtered_lap_list$LAP_NUM == lap_num),]$AVG_THROTTLE_POST_TURN2 = mean(postTurn2$THROTTLE)
    
    
    
  }
}

# Visualise the throttle patterns on the track (decreasing, increasing, same)

for (i in 1:497) {
  lap = findLap(i)
  plot(filtLeft$WORLDPOSX, filtLeft$WORLDPOSY, cex = 0.1)
  points(filtRight$WORLDPOSX, filtRight$WORLDPOSY, cex = 0.1)
  lines(x = x_start,y=y_start,col = "red", lwd=5)
  lines(x = x_finish,y=y_finish,col = "red", lwd=5)
  for (j in 1:nrow(lap)) {
    frame = lap[j,]
    if (frame$INCREASING_THROTTLE == TRUE) {
      points(frame$WORLDPOSX, frame$WORLDPOSY,  col = ('green'), cex = 0.1)
    } else if (frame$SAME_THROTTLE == TRUE) {
      points(frame$WORLDPOSX, frame$WORLDPOSY,  col = ('yellow'), cex = 0.1)
    } else {
      points(frame$WORLDPOSX, frame$WORLDPOSY,  col = ('red'), cex = 0.1)
    }
  }
  title(main = paste("Lap", i))
}

