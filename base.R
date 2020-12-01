# Base
library(data.table)
library(scorecard)

file_path <- "D:\\School\\Grad\\R\\Project\\kawhi_Leonard.csv"
# Output_path <- "D:\\School\\Grad\\R\\Project\\harden_prep.csv"

# -------------------

# Step 1: Load Data 
raw <- read.csv(file_path, header = TRUE)


# Step 2: Remove uneeded attributes 
# reduced <- raw[, -c(1,2,3,4,5,6,8,9,10)]  # Delete 1-10 but keep 7
reduced <- raw[, -c(1:10)]  # Delete 1-10 but keep 7

# Step 3: Handle Incomplete Data
# For now I am just droppeing rows where there is at least 1 'inactive'.
base_df <- reduced[reduced$FG != 'Inactive',]
base_df <- reduced[reduced$FG != 'Did Not Dress',]

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

# The idea here is that we will use the previous game to predict the next game
# This is because we can only predict with the information we have at the time we need to predict
base_df$Future_Score <- shift(base_df$Fantasy_Score, n=1, fill=NA, type="lead")

# base_df$C_Opp <- shift(base_df$Opp, n=1, fill=NA, type="lead")

# Remove the last row -- this has an NA as there is not a future game
base_df <- base_df[!is.na(base_df$Future_Score),]

# We do not need the actual fantasy score since this is not what we are predicting on
#base_df <- base_df[, -c(22:22)]
base_df <- base_df[, -c(21:21)]


# Step 5: Add Context - Create 'velocity' scores
# The idea here is to use exponential decay on key metrics
create_velocity <- function(values,week){
  
  # W(t) = 1 / k^(-t/T)
  
  k <- 0.61803398875
  T_ <- 1/k 
  
  
  transformed_values <- c()
  for(i in 1:length(values)){
    weightInverse <- k^((-1*(week-i))/T_)
    weight <- 1/weightInverse
    tmp_value <- as.numeric(values[i])*weight
    transformed_values <- c(transformed_values,tmp_value)
  }
  
  velocity_Score <- mean(transformed_values)
  
  return(velocity_Score)
}

#for(row in 1:nrow(base_df)){
#  if(row>2){
#    base_df$FG.V[row] <- create_velocity(base_df$FG.[1:row-1],row)
#    base_df$TOV.V[row] <- create_velocity(base_df$TOV[1:row-1],row)
#    base_df$BLK.V[row] <- create_velocity(base_df$BLK[1:row-1],row)
#    base_df$STL.V[row] <- create_velocity(base_df$STL[1:row-1],row)
#  }
#  else if(row==2){  # If this is the second game, then the velocity is just the first
#    base_df$FG.V[row] <- base_df$FG.[1]
#    base_df$TOV.V[row] <- base_df$TOV[1]
#    base_df$BLK.V[row] <- base_df$BLK[1]
#    base_df$STL.V[row] <- base_df$STL[1]
#  }
#  else{  # If first game, then there is no velocity (may just want to delete)
#    base_df$FG.V[row] <- 0
#    base_df$TOV.V[row] <- 0
#    base_df$BLK.V[row] <- 0
#    base_df$STL.V[row] <- 0
#  }
#}


# Step 6: Run PCA (would need to drop opponent and then add back)
# TODO


# Step 7: 1-Hot encode the opponent field
#base_df <- one_hot(base_df, var_encode = c('Opp','C_Opp'))
base_df <- sapply( base_df, as.numeric ) # Convert Everything to numeric


# Step 8: Save Data for Modeling
# Not writing properly
# write.table(base_df, Output_path, sep="\t")







