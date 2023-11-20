cars2023 = read.csv("f1sim-data-2023.csv")
cars2022 = read.csv("f1sim-data-2022.csv")


cars2022["year"] = rep(2022, length(cars2022$SESSION_IDENTIFIER))
cars2023["year"] = rep(2023, length(cars2023$SESSION_IDENTIFIER))

cars = bind_rows(cars2022,cars2023)

rm(cars2022, cars2023)

print("RUN SUCCESSFULLY: constructingTrainTestSetv1.0.R")

# sessions = as.data.frame(unique(cars$SESSION_IDENTIFIER))
# 
# randorder = sessions[sample(1:nrow(sessions)),]
# 
# sessions$randorder = randorder
# 
# trainprop = 0.8
# testprop = 1 - trainprop
# 
# trainSessions = sessions$randorder[1:round(trainprop * length(sessions$randorder))]
# testSessions = sessions$randorder[(round(trainprop * length(sessions$randorder))+1):length(sessions$randorder)]
# 
# carsTrain = cars[which(cars$SESSION_IDENTIFIER %in% trainSessions),]
# carsTest = cars[which(cars$SESSION_IDENTIFIER %in% testSessions),]
