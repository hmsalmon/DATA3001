# Pedal Transition 
# 
# Note: original code was written in Python

# Load required libraries
library(dplyr)
library(readr)

options(warn=-1)
options(scipen = 999)  

# Read the apex locations
turns <- read_csv('data/f1sim-ref-turns.csv')

# Read in data
df <- read_csv('data/filtered_cars_2023.csv')

# Function for pedal transitioning
pedal_transition <- function(df) {
  
  all_sessions <- unique(df$SESSION_IDENTIFIER)
  hold_all_braking_for_t1_dfs <- list()
  hold_all_acceleration_out_of_t1_dfs <- list()
  
  for (session in all_sessions) {
    lap_nums <- unique(df[df$SESSION_IDENTIFIER == session, ]$LAP_NUM)
    
    for (lap in lap_nums) {
      temp <- df[df$SESSION_IDENTIFIER == session & df$LAP_NUM == lap, ]
      
      # Braking into turn 1
      braking_for_t1 <- temp[which(temp$THROTTLE < 0.9)[1], ]
      
      # Acceleration out of turn 1
      start_idx <- which(temp$THROTTLE != 1)[1]
      if (!is.na(start_idx)) {
        df_out_of_t1 <- temp[start_idx:nrow(temp), ]
        acceleration_out_of_t1 <- df_out_of_t1[df_out_of_t1$WORLDPOSX >= turns$APEX_X1, ]
        acceleration_out_of_t1 <- acceleration_out_of_t1[which(acceleration_out_of_t1$THROTTLE > 0)[1], ]
        
        hold_all_braking_for_t1_dfs[[length(hold_all_braking_for_t1_dfs) + 1]] <- braking_for_t1
        hold_all_acceleration_out_of_t1_dfs[[length(hold_all_acceleration_out_of_t1_dfs) + 1]] <- acceleration_out_of_t1
      }
      
    }
  }
  
  t1_braking <- do.call(rbind, hold_all_braking_for_t1_dfs)
  out_of_t1 <- do.call(rbind, hold_all_acceleration_out_of_t1_dfs)
  
  return(list(t1_braking = t1_braking, out_of_t1 = out_of_t1))
}

# Call the function 
result <- pedal_transition(df)
t1_braking <- result$t1_braking
out_of_t1 <- result$out_of_t1

# Display the results
print(t1_braking)
print(out_of_t1)

# Simultaneous brake and throttle df 
brake_and_throttle <- df[df$THROTTLE > 0 & df$BRAKE > 0, ]
print(brake_and_throttle)
