#THIS FILE CAN RUN THE ENTIRE PROJECT CODE AND PRODUCE A TRAINING DATA SET

#Needs the original data files
source("constructingTrackSidesv1.1.R")

#Needs cars, filtLeft, filtRight
source("projectDataCleaningv1.3.R")

#Needs filtCars, filtLeft, filtRight
source("findStart&Finishv2.0.R")

#Needs filtCars (with normalised time), filtLeft, filtRight
source("constructingVariablesv1.0.R")
