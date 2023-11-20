getwd()
library(dplyr) #INSTALL THIS PACKAGE IF YOU HAVENT GOT IT ALREADY

#LOAD IN FILES (SET WORKING DIRECTORY TO FOLDER WITH THESE FILES IN IT)
turns = read.csv("f1sim-ref-turns.csv")
right = read.csv("f1sim-ref-right.csv")
left = read.csv("f1sim-ref-left.csv")
track = read.csv("f1sim-ref-line.csv")
cars = read.csv("f1sim-data-2022.csv")

# Using same start line and finish line, filter the data:

x_start = c(120.4416, 110.9773)
y_start = c(463.2061, 453.3787)

x_finish = c(641.6497,  634.1591)
y_finish = c(-221.5224, -226.8029)

start_line_intercept = summary(lm(y_start ~ x_start))$coefficients[1,1]
start_line_gradient = summary(lm(y_start ~ x_start))$coefficients[2,1]

finish_line_intercept = summary(lm(y_finish ~ x_finish))$coefficients[1,1]
finish_line_gradient = summary(lm(y_finish ~ x_finish))$coefficients[2,1]

cars$AboveStart <- (cars$WORLDPOSY > start_line_intercept + start_line_gradient * cars$WORLDPOSX)
cars$AboveFinish <- (cars$WORLDPOSY > finish_line_intercept + finish_line_gradient * cars$WORLDPOSX)

filtCars = cars[which(cars$AboveStart == FALSE & cars$AboveFinish == TRUE & cars$SECTOR == 0),]

#PLOT FOR SANTIY CHECK OF POINTS
plot(filtCars$WORLDPOSX, filtCars$WORLDPOSY, cex = 0.01)

# CREATE LAP LIST -
lapList = 
  
  filtCars %>%
  group_by(SESSION_IDENTIFIER, LAP_NUM) %>%
  slice(which.max(CURRENT_LAP_TIME_MS))

lapList = arrange(lapList, CURRENT_LAP_TIME_MS)

#EXTRACT DATA FOR LAP WITH THE NTH FASTEST TIME FOR RANGE
findLap <- function(rank) {
  
  sel_sessionid = lapList$SESSION_IDENTIFIER[rank]
  sel_lapnumid = lapList$LAP_NUM[rank]
  
  return(filtCars[filtCars$SESSION_IDENTIFIER == sel_sessionid & filtCars$LAP_NUM == sel_lapnumid,])
  
}


# Calculate time between frames
filtCars$TIME_SINCE_LAST_FRAME = 0
for (i in 1:nrow(lapList)) {
  current_lap = findLap(i)
  filtCars[which(filtCars$SESSION_IDENTIFIER == unique(current_lap$SESSION_IDENTIFIER) & filtCars$LAP_NUM == unique(current_lap$LAP_NUM)),] = filtCars[which(filtCars$SESSION_IDENTIFIER == unique(current_lap$SESSION_IDENTIFIER) & filtCars$LAP_NUM == unique(current_lap$LAP_NUM)),] %>%  mutate(TIME_SINCE_LAST_FRAME = CURRENT_LAP_TIME_MS - lag(CURRENT_LAP_TIME_MS, ))
}
filtCars["TIME_SINCE_LAST_FRAME"][is.na(filtCars["TIME_SINCE_LAST_FRAME"])] <- 0

# REMOVE LAPS WHERE THERE ARE LARGE GAPS IN THE DATA
badlaps = c()

remove_lap = FALSE
for (i in 1:nrow(lapList)) {
  remove_lap = FALSE
  current_lap = findLap(i)

  if (nrow(current_lap) == 1) {
    print(paste('Removing lap', i))
    badlaps = c(badlaps, i)
    cars = cars[!(cars$SESSION_IDENTIFIER == unique(current_lap$SESSION_IDENTIFIER) &
                            cars$LAP_NUM == unique(current_lap$LAP_NUM)),]
    filtCars = filtCars[!(filtCars$SESSION_IDENTIFIER == unique(current_lap$SESSION_IDENTIFIER) &
                            filtCars$LAP_NUM == unique(current_lap$LAP_NUM)),]
    next
  }
  # Remove if large gap between start line and 1st frame

  if (min(current_lap$LAP_DISTANCE) > 300) {
    remove_lap = TRUE
  }

  # Remove if large gap between consecutive frames

  if (max(current_lap$TIME_SINCE_LAST_FRAME) > 1000) {
    remove_lap = TRUE
  }

  # Remove if large gap between final frame and finish line

  if (max(current_lap$LAP_DISTANCE) < 600) {
    remove_lap = TRUE
  }

  if (remove_lap == TRUE) {
    print(paste('Removing lap', i))
    badlaps = c(badlaps, i)
    cars = cars[!(cars$SESSION_IDENTIFIER == unique(current_lap$SESSION_IDENTIFIER) &
                            cars$LAP_NUM == unique(current_lap$LAP_NUM)),]
    filtCars = filtCars[!(filtCars$SESSION_IDENTIFIER == unique(current_lap$SESSION_IDENTIFIER) &
                            filtCars$LAP_NUM == unique(current_lap$LAP_NUM)),]
  }
}

# Recreate lapList

lapList = 
  
  filtCars %>%
  group_by(SESSION_IDENTIFIER, LAP_NUM) %>%
  slice(which.max(CURRENT_LAP_TIME_MS))

lapList = arrange(lapList, CURRENT_LAP_TIME_MS)

#EXTRACT DATA FOR LAP WITH THE NTH FASTEST TIME FOR RANGE
findLap <- function(rank) {
  
  sel_sessionid = lapList$SESSION_IDENTIFIER[rank]
  sel_lapnumid = lapList$LAP_NUM[rank]
  
  return(filtCars[filtCars$SESSION_IDENTIFIER == sel_sessionid & filtCars$LAP_NUM == sel_lapnumid,])
  
}

# Visually inspect the laps we still have

for (i in 1:nrow(lapList)) {
  lap = findLap(i)
  plot(filtLeft$WORLDPOSX, filtLeft$WORLDPOSY, cex = 0.1)
  points(filtRight$WORLDPOSX, filtRight$WORLDPOSY, cex = 0.1)
  lines(x = x_start,y=y_start,col = "red", lwd=5)
  lines(x = x_finish,y=y_finish,col = "red", lwd=5)
  points(lap$WORLDPOSX, lap$WORLDPOSY, col = 'green', cex = 0.1)
  title(main = paste("Lap", i))
}

