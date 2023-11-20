
library(purrr)
library(dplyr)
filtCars = read.csv("filtCars_inRegionv1.1.csv")
filtLeft = read.csv("filtered_left.csv")
filtRight = read.csv("filtered_right.csv")
lap_data = read.csv("trainingDatav2.1.csv")

# Drop duplicated rows
filtCars <- subset(filtCars, select = -c(TIME_SINCE_LAST_FRAME, INCREASING_THROTTLE, DECREASING_THROTTLE, SAME_THROTTLE),)
filtCars <- unique(filtCars)


# Assign frames to segments of 10m
filtCars$SEGMENT <- cut(filtCars$LAP_DISTANCE_TOTAL, breaks = seq(0, max(filtCars$LAP_DISTANCE_TOTAL), by = 10), labels = FALSE)

# Create a list of dataframes each representing a lap and create a lapID column
laps_list <- list()
filtCars$LapID = -1


for (i in 1:nrow(lap_data)) {
  current_lap = lap_data[i,]
  
  current_lap_df <- filtCars[which(filtCars$SESSION_IDENTIFIER == current_lap$SESSION_IDENTIFIER & filtCars$LAP_NUM == current_lap$LAP_NUM),]
  df_name = paste0('Lap_', as.character(i))
  
  filtCars[which(filtCars$SESSION_IDENTIFIER == current_lap$SESSION_IDENTIFIER & filtCars$LAP_NUM == current_lap$LAP_NUM),]$LapID = i
  current_lap_df$ValidLap = tail(current_lap, 1)$ValidLap
  laps_list[[df_name]] <- current_lap_df
}

# Create a dataframe of all the relevant variables at the start of each segment for all laps 
segments <- data.frame()#

for (lapID in 1:length(laps_list)) {
  df_name <- paste0('Lap_', as.character(lapID))
  current_lap <- laps_list[[df_name]]
  
  # approximate variables at lap distances at increment of 10 meters
  distances <- seq(10, 900, by = 10)
  times_at_distances <- approx(current_lap$LAP_DISTANCE_TOTAL, current_lap$NORMALISED_LAP_TIME_MS, xout = distances)$y
  WORLDPOSX <- approx(current_lap$LAP_DISTANCE_TOTAL, current_lap$WORLDPOSX, xout = distances)$y
  WORLDPOSY <- approx(current_lap$LAP_DISTANCE_TOTAL, current_lap$WORLDPOSY, xout = distances)$y
  BRAKE <- approx(current_lap$LAP_DISTANCE_TOTAL, current_lap$BRAKE, xout = distances)$y
  GEAR <- approx(current_lap$LAP_DISTANCE_TOTAL, current_lap$GEAR, xout = distances)$y
  LATERAL <- approx(current_lap$LAP_DISTANCE_TOTAL, current_lap$LATERAL, xout = distances)$y
  THROTTLE <- approx(current_lap$LAP_DISTANCE_TOTAL, current_lap$THROTTLE, xout = distances)$y
  SPEED_KPH <- approx(current_lap$LAP_DISTANCE_TOTAL, current_lap$SPEED_KPH, xout = distances)$y
  ENGINE_RPM <- approx(current_lap$LAP_DISTANCE_TOTAL, current_lap$ENGINE_RPM, xout = distances)$y
  STEERING <- approx(current_lap$LAP_DISTANCE_TOTAL, current_lap$STEERING, xout = distances)$y
  
  
  

  # calculate differences
  
  differences <- diff(times_at_distances)
  if (any(differences < 0)) {
    next
  }
  # Create a data frame
  df <- data.frame(segment = distances, lapID = lapID, times_at_distances = times_at_distances, differences = c(NA, differences), WORLDPOSX = WORLDPOSX, WORLDPOSY = WORLDPOSY, THROTTLE = THROTTLE, BRAKE = BRAKE, GEAR = GEAR,LATERAL = LATERAL,SPEED_KPH = SPEED_KPH, ENGINE_RPM = ENGINE_RPM, STEERING = STEERING)
  df["differences"][is.na(df["differences"])] <- head(df$times_at_distances, 1)
  df$TOTAL_LAP_TIME = tail(current_lap, 1)$NORMALISED_LAP_TIME_MS
  df$ValidLap = tail(current_lap, 1)$ValidLap
  segments <- rbind(segments, df)
}

segments_excluding_invalid <- segments[which(segments$ValidLap == 1),]

# Group by segment
segments_grouped <- segments %>% group_split(segment) %>% map(~arrange(., differences))
segments_grouped_excluding_invalid <- segments_excluding_invalid %>% group_split(segment) %>% map(~arrange(., differences))



# Get the top 10 per segment 
top_10_per_seg <- lapply(segments_grouped, function(tibble) {
  tibble %>%
    arrange(differences) %>%
    slice_head(n = 10)
  
})

top_10_per_seg_excluding_invalid <- lapply(segments_grouped_excluding_invalid, function(tibble) {
  tibble %>%
    arrange(differences) %>%
    slice_head(n = 10)
  
})

top_10_overall <- lapply(segments_grouped, function(tibble) {
  tibble %>%
    arrange(lapID) %>%
    slice_head(n = 10)
})

# Determine effect of size of averaging group on lap time

different_avg_groups = data.frame()

for (i in 1:200) {
  # Get the average of the top
  top_i_per_seg_avg <- lapply(segments_grouped, function(tibble) {
    tibble %>%
      arrange(differences) %>%
      slice_head(n = i) %>% 
      summarise(across(where(is.numeric), mean))
  })
  all_differences_best <- unlist(lapply(top_i_per_seg_avg, function(tibble) tibble$differences))
  best_lap_time = sum(all_differences_best)
  df <- data.frame(num_frames = i, best_lap_time = best_lap_time)
  different_avg_groups <- rbind(different_avg_groups, df)
}

plot(different_avg_groups$num_frames, different_avg_groups$best_lap_time, xlab = 'Number of Frames Used in Average', ylab='Best Lap Time')


# Get the average of the top 10
top_10_per_seg_avg <- lapply(segments_grouped, function(tibble) {
  tibble %>%
    arrange(differences) %>%
    slice_head(n = 10) %>% 
    summarise(across(where(is.numeric), mean))
  
})

top_10_per_seg_avg_excluding_invalid <- lapply(segments_grouped_excluding_invalid, function(tibble) {
  tibble %>%
    arrange(differences) %>%
    slice_head(n = 10) %>% 
    summarise(across(where(is.numeric), mean))
  
})

top_10_overall_avg <- lapply(segments_grouped_excluding_invalid, function(tibble) {
  tibble %>%
    arrange(lapID) %>%
    slice_head(n = 10) %>% 
    summarise(across(where(is.numeric), mean))
})


best_lap <- do.call(rbind, top_10_per_seg_avg)
best_lap_excluding_invalid <- do.call(rbind, top_10_per_seg_avg_excluding_invalid)
best_lap_with_overall_best <- do.call(rbind, top_10_overall_avg)



# Plot the best lap
plot(filtLeft$WORLDPOSX, filtLeft$WORLDPOSY, cex = 0.01, xlab = 'WORLDPOSX', ylab = 'WORLDPOSY')
points(filtRight$WORLDPOSX, filtRight$WORLDPOSY, cex = 0.01)
lines(best_lap_excluding_invalid$WORLDPOSX, best_lap_excluding_invalid$WORLDPOSY, col = 'red',lwd=2)
#points(lap1$WORLDPOSX, lap1$WORLDPOSY, col = 'blue', cex = 0.1)

# Compare average overall lap time per segments selected

plot(best_lap$segment, best_lap$TOTAL_LAP_TIME, type = 'l', ylim = c(12000, 22000), xlab='Lap Distance', ylab='Average Total Lap Time')
lines(best_lap_excluding_invalid$segment, best_lap_excluding_invalid$TOTAL_LAP_TIME, col = 'red')
lines(best_lap_with_overall_best$segment, best_lap_with_overall_best$TOTAL_LAP_TIME, col = 'green')
legend("topright", legend = c("Including Invalid Laps", "Excluding Invalid Laps", "Top Valid Laps"), col = c("black", "red", "green"), lwd = 2)


plot(best_lap$segment, best_lap$differences, type = 'l', ylim = c(0, 250), xlab='Lap Distance', ylab='Average Time Per Segment')
lines(best_lap_excluding_invalid$segment, best_lap_excluding_invalid$differences, col = 'red')
lines(best_lap_with_overall_best$segment, best_lap_with_overall_best$differences, col = 'green')
legend("topright", legend = c("Including Invalid Laps", "Excluding Invalid Laps", "Top Valid Laps"), col = c("black", "red", "green"), lwd = 2)


# The above plot revealed that being too fast in segments 34-37 is not good - investigate

plot(filtCars_OnTrack$WORLDPOSX, filtCars_OnTrack$WORLDPOSY, cex = 0.01, xlab = 'WORLDPOSX', ylab = 'WORLDPOSY')
points(segments_grouped_excluding_invalid[[34]]$WORLDPOSX, segments_grouped_excluding_invalid[[34]]$WORLDPOSY, cex = 0.5, col = 'red')
points(segments_grouped_excluding_invalid[[35]]$WORLDPOSX, segments_grouped_excluding_invalid[[35]]$WORLDPOSY, cex = 0.5, col = 'red')
points(segments_grouped_excluding_invalid[[36]]$WORLDPOSX, segments_grouped_excluding_invalid[[36]]$WORLDPOSY, cex = 0.5, col = 'red')
points(segments_grouped_excluding_invalid[[37]]$WORLDPOSX, segments_grouped_excluding_invalid[[37]]$WORLDPOSY, cex = 0.5, col = 'red')

time_per_seg_differences <- best_lap_with_overall_best$differences - best_lap_excluding_invalid$differences

# recalculate best lap by not including laps which are extremely slow overall for these segments

for (i in 34:37) {
  current_seg = top_10_per_seg_excluding_invalid[[i]]
  if ((mean(top_10_overall[[i]]$differences) - mean(current_seg$differences)) > 15) {
    slowest_lap_in_seg <- max(current_seg$lapID)
    current_segment_group <- segments_grouped_excluding_invalid[[i]]
    current_segment_group <- filter(current_segment_group, lapID != slowest_lap_in_seg)
    print(current_segment_group)
    segments_grouped_excluding_invalid[[i]] <- current_segment_group
  }
}

top_10_per_seg_avg_excluding_invalid <- lapply(segments_grouped_excluding_invalid, function(tibble) {
  tibble %>%
    arrange(differences) %>%
    slice_head(n = 10) %>% 
    summarise(across(where(is.numeric), mean))
  
})

best_lap_excluding_invalid <- do.call(rbind, top_10_per_seg_avg_excluding_invalid)

# Plot the best lap again
plot(filtLeft$WORLDPOSX, filtLeft$WORLDPOSY, cex = 0.01, xlab = 'WORLDPOSX', ylab = 'WORLDPOSY')
points(filtRight$WORLDPOSX, filtRight$WORLDPOSY, cex = 0.01)
lines(best_lap_excluding_invalid$WORLDPOSX, best_lap_excluding_invalid$WORLDPOSY, col = 'red',lwd=2)

# Get the average of the worst
bottom_10_per_seg_avg <- lapply(segments_grouped, function(tibble) {
  tibble %>%
    arrange(differences) %>%
    slice_tail(n = 10) %>% 
    summarise(across(where(is.numeric), mean))
  
})

# Get the average of the worst excluding invalid laps
bottom_10_per_seg_avg_excluding_invalid <- lapply(segments_grouped_excluding_invalid, function(tibble) {
  tibble %>%
    arrange(differences) %>%
    slice_tail(n = 10) %>% 
    summarise(across(where(is.numeric), mean))
  
})

# Plot the worst lap
worst_lap_excluding_invalid <- do.call(rbind, bottom_10_per_seg_avg_excluding_invalid)
lines(worst_lap_excluding_invalid$WORLDPOSX, worst_lap_excluding_invalid$WORLDPOSY, col = 'blue',lwd=2)


# Calculate lap time for best lap excluding invalid laps

all_differences_best <- unlist(lapply(top_10_per_seg_avg_excluding_invalid, function(tibble) tibble$differences))
best_lap_time = sum(all_differences_best)

# Calculate lap time for worst lap excluding invalid laps

all_differences_worst <- unlist(lapply(bottom_10_per_seg_avg_excluding_invalid, function(tibble) tibble$differences))
worst_lap_time = sum(all_differences_worst)


# title(main = "Best vs Worst Lap Comparison")
legend("topright", legend = c("Worst Lap", "Best Lap"), col = c("blue", "red"), lwd = 2)



# Construct the best_lap dataframe
best_lap_excluding_invalid$times_at_distances = cumsum(best_lap_excluding_invalid$differences)

names(best_lap_excluding_invalid)[names(best_lap_excluding_invalid) == "segment"] <- "LAP_DISTANCE"
names(best_lap_excluding_invalid)[names(best_lap_excluding_invalid) == "times_at_distances"] <- "LAP_TIME"

best_lap_excluding_invalid$GEAR <- round(best_lap_excluding_invalid$GEAR)
best_lap_excluding_invalid$SPEED_KPH <- round(best_lap_excluding_invalid$SPEED_KPH)
best_lap_excluding_invalid$ENGINE_RPM <- round(best_lap_excluding_invalid$ENGINE_RPM)

best_lap_excluding_invalid <-  subset(best_lap_excluding_invalid, select = -c(lapID, differences))


# Construct the worst_lap dataframe
worst_lap_excluding_invalid$times_at_distances = cumsum(worst_lap_excluding_invalid$differences)

names(worst_lap_excluding_invalid)[names(worst_lap_excluding_invalid) == "segment"] <- "LAP_DISTANCE"
names(worst_lap_excluding_invalid)[names(worst_lap_excluding_invalid) == "times_at_distances"] <- "LAP_TIME"

worst_lap_excluding_invalid$GEAR <- round(worst_lap_excluding_invalid$GEAR)
worst_lap_excluding_invalid$SPEED_KPH <- round(worst_lap_excluding_invalid$SPEED_KPH)
worst_lap_excluding_invalid$ENGINE_RPM <- round(worst_lap_excluding_invalid$ENGINE_RPM)

worst_lap_excluding_invalid <-  subset(worst_lap_excluding_invalid, select = -c(lapID, differences))



# Plot frequencies of laps used
Frequencies <- as.data.frame(table(all_top_10$lapID))

names(Frequencies)[names(Frequencies) == "Var1"] <- "LapID"
Frequencies <- Frequencies[order(-Frequencies$Freq), ]
top_10_lapIDs <- head(Frequencies$LapID, 10)
barplot(Frequencies$Freq[Frequencies$LapID %in% top_10_lapIDs], names.arg = top_10_lapIDs, col = "skyblue",
        main = "Most Common Laps Used", xlab = "LapID", ylab = "Frequency", ylim = c(0, 33))

bottom_10_lapIDs <- tail(Frequencies$LapID, 10)
barplot(Frequencies$Freq[Frequencies$LapID %in% bottom_10_lapIDs], names.arg = bottom_10_lapIDs, col = "skyblue",
        main = "Least Common Laps Used", xlab = "LapID", ylab = "Frequency", ylim = c(0, 33))



# Plot Best Lap Steering Pattern

plot(best_lap_excluding_invalid$LAP_DISTANCE, best_lap_excluding_invalid$STEERING, xlab = 'Lap Distance', ylab = 'Steering Percentage')
text(290, 0.018, print("Start: (290m, 0.018%)"), pos = 3, col = "red")
text(380, 0.49, print("Max: (380m, 0.49%)"), pos = 3, col = "red")
text(500, -0.20, print("Min: (500m, -0.20%)"), pos = 4, col = "red")
text(610, -0.01, print("Finish: (610m, -0.01%)"), pos = 4, col = "red")
title(main = 'Optimal Steering Pattern')

# Plot Best Lap Throttle vs Brake

plot(best_lap_excluding_invalid$LAP_DISTANCE, best_lap_excluding_invalid$THROTTLE, type = 'l', col = 'green', lwd = '2', xlab = 'Lap Distance', ylab = 'Percentage Applied')
lines(best_lap_excluding_invalid$LAP_DISTANCE, best_lap_excluding_invalid$BRAKE, col = 'red', lwd = '2')
legend("topright", legend = c("Throttle", "Brake"), col = c("green", "red"), lwd = 2)
title(main = 'Optimal Throttle and Brake Pattern')

# Plotting Best Lateral Position
plot(best_lap_excluding_invalid$LATERAL, best_lap_excluding_invalid$LAP_DISTANCE, type = 'l', col = 'black', lwd = '2', xlab = 'Lateral Position', ylab = 'Lap Distance')
title(main = 'Optimal Lateral Positioning')

