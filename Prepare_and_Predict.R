# Imports
library(data.table)
library(scorecard)
library(randomForest)

set.seed(2020)
# ------

# Files
Train_file <- "D:\\School\\Grad\\R\\Final_Project\\Training_Data\\Boston Celtics\\Tatum.csv"  # 2017-2018 Season CSV (using Player Excel)
Test_file <- "D:\\School\\Grad\\R\\Final_Project\\Test_Data\\Boston Celtics\\Tatum.csv"       # 2017-2018 Season CSV (using Player Excel)
Output_path <- "D:\\School\\Grad\\R\\Final_Project\\Output\\Tatum.csv"                       # This will be combined to make master.csv

Team <- 'Boston Celtics'                                                                       # We need to manually put team
Player <- 'Tatum'                                                                            # Name of Player
# ------

# ---- Prepare Training ----

# Step 1: Load Data 
raw <- read.csv(Train_file, header = TRUE)

# Step 2: Remove uneeded attributes 
reduced <- raw[, -c(1,2,3,4,5,6,8,9,10)]  # Delete 1-10 but keep 7

# Step 3: Handle Incomplete Data
base_df <- reduced[reduced$FG != 'Inactive',]
base_df <- base_df[base_df$FG != 'Did Not Dress',]
base_df <- base_df[base_df$FG != 'Did Not Play',]
base_df <- base_df[base_df$FG != 'Not With Team',]


# Step 4: Calculate the Fantasy score
calculate_fantasy <- function(df){
  # ~~~~~ From: https://fantasydata.com/api/fantasy-scoring-system/nba ~~~~~
  # Three Point Field Goals: 3 points
  # Two Point Field Goals: 2 points
  # Free Throws Made: 1 point
  # Rebounds: 1.2 points
  # Assists: 1.5 points
  # Blocked Shots: 2 points
  # Steals: 2 points
  # Turnovers: -1 points
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Convert to list so I can use $ Indexing
  df <- as.list(df)
  
  fantasy_Score = (as.numeric(df$X3P) * 3)+(as.numeric(df$FG) * 2)+(as.numeric(df$FT) * 1)
  +(as.numeric(df$TRB) * 1.2)+(as.numeric(df$AST) * 1.5)+(as.numeric(df$BLK) * 2)+(as.numeric(df$STL) * 2)-(as.numeric(df$TOV) * -1)
  
  return(fantasy_Score)
}

base_df$Fantasy_Score <- apply(base_df,1,FUN=calculate_fantasy)

# Step 5: Shift Values
# The idea here is that we will use the previous game to predict the next game
# This is because we can only predict with the information we have at the time we need to predict
base_df$Future_Score <- shift(base_df$Fantasy_Score, n=1, fill=NA, type="lead")
# We also want to know the team we are about to predict for
base_df$C_Opp <- shift(base_df$Opp, n=1, fill=NA, type="lead")

# Step 6: Reduce DF
# Remove the last row -- this has an NA as there is not a future game
base_df <- base_df[!is.na(base_df$Future_Score),]

# We do not need the actual fantasy score since this is not what we are predicting on
base_df <- base_df[, -c(22:22)]

# Step 7: 1-Hot encode the opponent field
base_df <- one_hot(base_df, var_encode = c('Opp','C_Opp'))
base_df <- sapply( base_df, as.numeric ) # Convert Everything to numeric

# ---- Create Model ----
# Create 70/30 split
train <- sample(nrow(base_df), 0.7*nrow(base_df), replace = FALSE)
TrainSet <- base_df[train,]
ValidSet <- base_df[-train,]


meta_data  <- list() 
i<-1
for(number_Trees in c(400,500,600,700,800,800,1000)){
  for(Mtrys in c(3,4,5,6,7,8,9,10)){
    for(imp in c(TRUE,FALSE)){
      # print(number_Trees)
      # print(Mtrys)
      # print(imp)
      
      # We Can fine tune parameters (hyperparameterization) like this and read up using ?randomForest --- improvment for later
      model2 <- randomForest(Future_Score ~ ., data = TrainSet, ntree = number_Trees, mtry = Mtrys, importance = imp)
      
      # Predict on Train
      predTrain <- predict(model2, TrainSet)
      # print(length(predTrain) == length(TrainSet[,21]))
      Train_errors = abs(TrainSet[,21]-predTrain)
      
      Train_Mean_asboluteError = mean(Train_errors)
      # print(paste(c("[Train] Mean Absolute Error (in fantasy points): ", Train_Mean_asboluteError), collapse=" "))
      
      Train_mape = 100 * (Train_errors/TrainSet[,21])
      Train_Accuracy = 100 - mean(Train_mape)
      # print(paste(c("[Train] Accuracy: ", Train_Accuracy), collapse=" "))
      
      # --------------------------------------------------------------------
      
      # Predict on Test
      predTest <- predict(model2, ValidSet)
      # print(length(predTest) == length(ValidSet[,21]))
      Test_errors = abs(ValidSet[,21]-predTest)
      
      Test_Mean_asboluteError = mean(Test_errors)
      # print(paste(c("[Test] Mean Absolute Error (in fantasy points): ", Test_Mean_asboluteError), collapse=" "))
      
      Test_mape = 100 * (Test_errors/ValidSet[,21])
      Test_Accuracy = 100 - mean(Test_mape)
      # print(paste(c("[Test] Accuracy: ", Test_Accuracy), collapse=" "))
      meta_data[[i]] <- c(number_Trees,Mtrys,imp,Train_Mean_asboluteError,Train_Accuracy,Test_Mean_asboluteError,Test_Accuracy)
      i <- i+1
      # print('---------------------------')
    }
  }
}
print("Done")
meta_df = as.data.frame(do.call(rbind, meta_data))
colnames(meta_df) <- c('ntree','mtry','importance','Train_Mean_asboluteError','Train_Accuracy','Test_Mean_asboluteError','Test_Accuracy')

# ---- Prepare Test ----

# Step 1: Load Data 
raw_test <- read.csv(Test_file, header = TRUE)

# Step 2: Remove uneeded attributes 
reduced_test <- raw_test[, -c(1,2,3,4,5,6,8,9,10)]  # Delete 1-10 but keep 7



# Step 3: Handle Incomplete Data
base_df_test <- reduced_test[reduced_test$FG != 'Inactive',]
base_df_test <- base_df_test[base_df_test$FG != 'Did Not Dress',]
base_df_test <- base_df_test[base_df_test$FG != 'Did Not Play',]
base_df_test <- base_df_test[base_df_test$FG != 'Not With Team',]


# Step 5: Shift Values
# We want to know what the current team we are facing is 
base_df_test$C_Opp <- shift(base_df_test$Opp, n=1, fill=NA, type="lead")

# Step 6: Reduce DF
base_df_test <- base_df_test[!is.na(base_df_test$C_Opp),]

# Step 7: 1-Hot encode the opponent field
base_df_test <- one_hot(base_df_test, var_encode = c('Opp','C_Opp'))
# Check Column Mismatch
train_columns <- colnames(base_df)
test_columns <- colnames(base_df_test)
if(length(train_columns)>length(test_columns)){
  print("2019 missing game data. Adding default values.")
  for(col in train_columns){
    if(!(col%in%test_columns)){
      if(col!="Future_Score"){
        print(col)
        base_df_test[, col] <- 0    
      }
    }
  }
} else if(length(train_columns)<length(test_columns)){
  print("Extra Columns")
} else{
  print("Same Length - All Good")
}
# ------
base_df_test <- sapply( base_df_test, as.numeric ) # Convert Everything to numeric

# ---- Use Model ----
if(meta_df[meta_df['Test_Accuracy']==max(meta_df$Test_Accuracy)][3]==1.000000){
  importance_var <- TRUE  
} else{
  importance_var <- FALSE 
}

model_2018 <- randomForest(Future_Score ~ ., data = TrainSet, ntree = meta_df[meta_df['Test_Accuracy']==max(meta_df$Test_Accuracy)][1], mtry = meta_df[meta_df['Test_Accuracy']==max(meta_df$Test_Accuracy)][2], importance = importance_var)

predictions_2019 <- predict(model_2018, base_df_test)

# ---- Make Output ----

output_df <- raw_test[raw_test$FG != 'Inactive',]
output_df <- output_df[output_df$FG != 'Did Not Dress',]
output_df <- output_df[output_df$FG != 'Did Not Play',]
output_df <- output_df[output_df$FG != 'Not With Team',]

output_df$C_Opp <- shift(output_df$Opp, n=1, fill=NA, type="lead")

output_df <- output_df[!is.na(output_df$C_Opp),]

output_df$Fantasy_Score <- apply(output_df,1,FUN=calculate_fantasy)

output_df <- output_df[, c(3,7,32)]  # Delete 1-10 but keep 7

output_df <- cbind(output_df,predictions_2019)

output_df$predictions_2019 <- shift(output_df$predictions_2019, n=1, fill=NA, type="lag")

output_df$Team <- Team

output_df$Player <- Player

output_df <- output_df[,c('Player','Team','Date','Opp','Fantasy_Score','predictions_2019')]

write.csv(output_df, file=Output_path)