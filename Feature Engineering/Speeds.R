# Speeds 
# 
# Note: original code was written in Python

# Load libraries
library(readr)
library(dplyr)
library(tidyr)

options(warn=-1)

# Read data
turns <- read_csv('data/f1sim-ref-turns.csv')
turns <- filter(turns, TURN %in% c(1, 2))

df <- read_csv('data/filtered_cars_2023.csv')

# Calculate ENGINE_RPM_PERCENT
max_rpm <- max(df$ENGINE_RPM)
df$ENGINE_RPM_PERCENT <- df$ENGINE_RPM / max_rpm

# Calculate GEAR_PERCENT
max_gear <- max(df$GEAR)
df$GEAR_PERCENT <- df$GEAR / max_gear

# Define a function to calculate speed at t1
# Define a function to calculate speed at t1
speed_at_t1 <- function(df) {
  all_sessions <- unique(df$SESSION_IDENTIFIER)
  speed_at_one <- data.frame()
  
  for (session in all_sessions) {
    lap_nums <- unique(subset(df, SESSION_IDENTIFIER == session)$LAP_NUM)
    for (lap in lap_nums) {
      temp <- subset(df, SESSION_IDENTIFIER == session & LAP_NUM == lap)
      temp$diff <- c(NA, diff(temp$c1dist))
      
      index <- which(temp$diff < 0 & lead(temp$diff) > 0)
      
      if (length(index) > 0) {
        index <- index[which.max(index)]
        
        dist_in <- temp$SPEED_KPH[index]
        dist_out <- temp$SPEED_KPH[index + 1]
        
        temp <- data.frame(SESSION_IDENTIFIER = session, LAP_NUM = lap, SPEED_KPH = (dist_in + dist_out) / 2)
        
        speed_at_one <- bind_rows(speed_at_one, temp)
      }
    }
  }
  
  return(speed_at_one)
}

spd_t1 <- speed_at_t1(df)

# Display the result
print(spd_t1)

# Define a function to calculate speed at t2
speed_at_t2 <- function(df) {
  all_sessions <- unique(df$SESSION_IDENTIFIER)
  speed_at_two <- data.frame()
  
  for (session in all_sessions) {
    lap_nums <- unique(subset(df, SESSION_IDENTIFIER == session)$LAP_NUM)
    for (lap in lap_nums) {
      temp <- subset(df, SESSION_IDENTIFIER == session & LAP_NUM == lap)
      temp$diff <- c(NA, diff(temp$c2dist))
      
      index <- which(temp$diff < 0 & lead(temp$diff) > 0)
      
      if (length(index) > 0) {
        index <- index[which.max(index)]
        
        dist_in <- temp$SPEED_KPH[index]
        dist_out <- temp$SPEED_KPH[index + 1]
        
        temp <- data.frame(SESSION_IDENTIFIER = session, LAP_NUM = lap, SPEED_KPH = (dist_in + dist_out) / 2)
        
        speed_at_two <- bind_rows(speed_at_two, temp)
      }
    }
  }
  
  return(speed_at_two)
}

spd_t2 <- speed_at_t2(df)

# Display the result
print(spd_t2)

