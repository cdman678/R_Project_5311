path = "D:\\School\\Grad\\R\\Final_Project\\Output\\"

master <- data.frame(NULL, stringsAsFactors = TRUE)

file.names <- dir(path, pattern =".csv")

for(i in 1:length(file.names)){
  file <- read.csv(paste(path,file.names[i],sep=''),header=TRUE)
  master <- rbind(master, file)
}

# Delete the Index that was carried over
master <- master[, -c(1)] 

# For Now I will delete the first game as we have no prediction
master <- master[!is.na(master$predictions_2019),]

# Manual: Convert the index to a player_ID
write.csv(master, file=paste(path,'Master.csv',sep=''))

# -------- VALIDATION ---------

players_found <- unique(master$Player)
print(paste("Number of Players:", length(players_found), sep=" "))

Teams_found <- unique(master$Team)
print(paste("Number of Teams:", length(Teams_found), sep=" "))
print(paste("Teams:", Teams_found, sep=" "))

# -------- Accuracy ---------
master$Error <- abs(master$Fantasy_Score-master$predictions_2019)

master$mape = 100 * (master$Error/master$Fantasy_Score)

master$Accuracy = 100 - master$mape

print(median(master$Accuracy))

# Point of Reference, from our investigation (using 2018) the median accuracy was 68.94


