
library(purrr)
library(dplyr)
filtCars = read.csv("filtCars_inTimedRangev1.1.csv")
filtLeft = read.csv("filtered_left.csv")
filtRight = read.csv("filtered_right.csv")
lap_data = read.csv("trainingDatav2.1 (1).csv")


# drop duplicated rows
filtCars <- subset(filtCars, select = -c(TIME_SINCE_LAST_FRAME, INCREASING_THROTTLE, DECREASING_THROTTLE, SAME_THROTTLE),)
filtCars <- unique(filtCars)


# Filter out any laps which go off the track at any point
bad_laps = lap_data[which(lap_data$ValidLap_4wheels== 0),] 
filtered_data <- anti_join(filtCars, bad_laps, by = c("SESSION_IDENTIFIER", "LAP_NUM"))
filtCars_OnTrack = filtered_data
good_laps = lap_data[which(lap_data$ValidLap_4wheels== 1),] 


# Create a list of dataframes each representing a lap and create a lapID column
laps_list <- list()
filtCars_OnTrack$LapID = -1

for (i in 1:nrow(good_laps)) {
  current_lap = good_laps[i,]
  current_lap_df <- filtCars_OnTrack[which(filtCars_OnTrack$SESSION_IDENTIFIER == current_lap$SESSION_IDENTIFIER & filtCars_OnTrack$LAP_NUM == current_lap$LAP_NUM),]
  df_name = paste0('Lap_', as.character(i))
  
  filtCars_OnTrack[which(filtCars_OnTrack$SESSION_IDENTIFIER == current_lap$SESSION_IDENTIFIER & filtCars_OnTrack$LAP_NUM == current_lap$LAP_NUM),]$LapID = i
  
  # Assigning the segment to its own data frame in the list segments_list
  laps_list[[df_name]] <- current_lap_df
}



segments <- data.frame()

for (lapID in 1:length(laps_list)) {
  df_name <- paste0('Lap_', as.character(lapID))
  current_lap <- laps_list[[df_name]]
  
  # predict times, worldposx and worldposy at lap distances at increment of 1 meter 
  distances <- seq(0, max(current_lap$LAP_DISTANCE_TOTAL), by = 1)
  times_at_distances <- approx(current_lap$LAP_DISTANCE_TOTAL, current_lap$NORMALISED_LAP_TIME_MS, xout = distances)$y
  WORLDPOSX_at_dist <- approx(current_lap$LAP_DISTANCE_TOTAL, current_lap$WORLDPOSX, xout = distances)$y
  WORLDPOSY_at_dist <- approx(current_lap$LAP_DISTANCE_TOTAL, current_lap$WORLDPOSY, xout = distances)$y
  
  # calculate differences
  differences <- diff(times_at_distances)
  # Create a data frame
  df <- data.frame(segment = distances, lapID = lapID, times_at_distances = times_at_distances, differences = c(NA, differences), worldposx = WORLDPOSX_at_dist, worldposy = WORLDPOSY_at_dist)
  df <- df[-1, , drop = FALSE]
  df["differences"][is.na(df["differences"])] <- head(df$times_at_distances, 1)
  segments <- rbind(segments, df)
}



segments_grouped <- segments %>% group_split(segment) %>% map(~arrange(., differences))

# top_20_laps <- map(segment_grouped_tibbles, ~ arrange(., differences) %>% slice_head(n = 20))

# Get the average of the top 50 lap positions with the lowest estimated time in each segment 


top_50_averages <- lapply(segments_grouped, function(tibble) {
  tibble %>%
    arrange(differences) %>%
    slice_head(n = 50) %>%
    summarise(across(where(is.numeric), mean))
})

plot(filtLeft$WORLDPOSX, filtLeft$WORLDPOSY, cex = 0.1, xlab = 'WORLDPOSX', ylab = 'WORLDPOSY')
points(filtRight$WORLDPOSX, filtRight$WORLDPOSY, cex = 0.1)
for (i in seq_along(top_50_averages)) {
  points(top_50_averages[[i]]$worldposx, top_50_averages[[i]]$worldposy, col = 'blue', cex = 0.1)
}

