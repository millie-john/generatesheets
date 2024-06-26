# Generate 20 trial sheets
# Written by Millie Johnston
# Version 0.3
# Last updated 2024-04-27

### Randomise hand movements
##-----------------------------------------------
# Trials   10
# Sample   R or L (no more than 2 in a row and equal distribution)
# Reward   R or L (no more than 2 in a row and equal distribution)
# Movement P or C (no more than 2 in a row and equal distribution)
# Final    R or L (no more than 2 in a row and equal distribution)
# 
# Sample and Reward (no more than 2 in a row and equal distribution)
# Sample, Reward, and Movement (no more than 2 in a row and equal distribution)
#
# Generate 2 sheets, then add labels 1 & 2, and then check for distribution again (no more than 2 in a row and equal distribution)
# Individual sheets are named `Iteration_10_<number>.csv`
# Final sheet is named `List_20_<number>.csv`
# Sheets will be generated in the folder location you are executing from, you can check this using `getwd()`
# You can change your working directory with `setwd()`, for example, `setwd("~/Desktop")`
#
# Packages are listed at the top and should auto-install if they are not already available.
##-----------------------------------------------

# Install packages if not already installed
list.of.packages <- c("tidyr",
                      "openxlsx",
                      "dplyr")

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

if (length(new.packages)) {
  install.packages(new.packages)
}

# Load packages
library(openxlsx)
library(tidyr)
library(dplyr)

# Create list of variables
data <- list(reward = c("R", "L"),
             move = c("P", "C"))

# Make a grid of possible list combinations
comb <- expand.grid(data)

# Add final positions to grid
finpos <- apply(comb[, 1:2], 1, function(x)
  paste0(x[1], x[2]))
finpos <-
  ifelse(finpos == "LP", "L", ifelse(finpos == "RP", "R", ifelse(finpos == "LC", "R", "L")))
comb <- cbind(comb, finpos)
comb <- comb[rep(seq_len(nrow(comb)), 3), ]

# Make a list for sample hand
sam <- list(sample = c("R", "L"))

# Make a grid of possible sample hand combinations
sam <- expand.grid(sam)

# Make into a data frame for easy replication and binding
sam <- cbind(sam)

# Replicate sample hand 6 times so it now equals 12
sam <- sam[rep(seq_len(nrow(sam)), 6), ]

# Function to check if conditions are represented in a sequence n or more times, in a column, returns true or false. Queries if the sequence "has consecutive", if false, it's a good thing.
has_consecutive <- function(vec, n) {
  any(sapply(rle(as.character(vec))$lengths, function(x)
    x >= n))
}

# Prepare for loop
j = 1 # Start of counter so you can stop it after generating "sheets" lists
df_comp <- data.frame() # Empty data frame to bind the lists
broke = FALSE

# Loop to find data frames that pass criteria
for (i in 1:5000000) {
  # Make a random data frame of 10 trials sampled from list, sample without replacement
  df <- comb %>% slice_sample(n = 12, replace = FALSE)
  
  # Check if sample, reward, or movement is represented equals 6, retures true of false
  if (any(apply(t(df), 1, function(x)
    any(!table(x) == 6)))) {
    next
  }
  
  # Check is any value in any column is consecutive more than > or = 3 times, returns true of false
  if (any(apply(df[, 1:3], 2, function(x)
    has_consecutive(x, 3)))) {
    next
  }
  
  # Nested loop to add sample hand, runs check to see if there are any runs of 3 or more for the first 2 columns (sample and reward) and if there is an even distribution of congruent and noncongruent trials, and if there is an even distribution of the first 3 columns.
  for (z in 1:1000) {
    df_s <- as.data.frame(sam) %>% slice_sample(n = 12, replace = FALSE)
    df_s <- cbind(df_s, df)
    if (any(apply(df_s[, 1:2], 2, function(x)
      has_consecutive(x, 3))) ||
      !all(table(paste(df_s[, 1], df_s[, 2], sep = "")) == 3) ||
      has_consecutive(df_s[,1] == df_s[,2], 3) ||
      !all(table(paste(df_s[, 1], df_s[, 2], df_s[, 3], sep = ""))  < 3) ||
      !length(table(paste(df_s[, 1], df_s[, 2], df_s[, 3], sep = ""))) == 8 )
    {
      next;
      if (z == 999 ) {
        broke = TRUE
        break
      }
    } else {
      break
    }
  }
  
  if (broke) {
    broke = FALSE;
    next
  }
  
  #Subset to 10
  df_s <- df_s[1:10,]
  
  if (!length(table(paste(df_s[, 1], df_s[, 2], df_s[, 3], sep = ""))) == 8 )
  {
    next
  } 
  
  # Five of the different types (RR, LL, RL, & LR)
  
  # Bind to df and add column names
  df <- cbind(df_s[, 1], df[1:10,])
  colnames(df) <- c("sample", "reward", "move", "finpos")
  
  # Add condition number to df
  df_temp <- cbind(df, j)
  
  # Merge lists
  df_comp <- rbind(df_comp, df_temp)
  
  # Report something was found
  print(paste("Found ", j, " (Iteration ", i, ")", sep = ""))
  
  # Add tick boxes and column names
  df_save <- cbind(1:10, df_temp, "[  ]", "[  ]", "", "[  ]", "")
  colnames(df_save) <-
    c("Trial",
      "Sample",
      "Reward",
      "Move",
      "End",
      "Condition",
      "L",
      "R",
      "",
      "✓",
      "")
  
  # Write to sheet and save in working directory
  write.csv(
    x = df_save,
    file = paste0("Iteration_10_", i, ".csv"),
    row.names = FALSE
  )
  
  # Add 1 to counter
  j = j + 1
  
  # Break once counter is greater than 2
  if (j > 2) {
    break
  }
  
}

# Make a copy and remove first two rows

## Condition 1 large jar is rewarding (L or R)
## Condition 2 large jar is opposite or rewarding (L or R)
## if condition = 1 then large jar = rewarding
## if condition = 2 then large jar = opposite of rewarding

df_copy <- df_comp

new_df <- df_copy[1:2,]
df_copy <- df_copy[-(1:2),]

rs_new_df <- new_df
rs_df_copy <- df_copy

i = 1

print("Solving conditions...")

# Start a while loop - this will run until it finds a solution
while (nrow(df_copy) > 0) {
  # Make row of interest
  roi <- df_copy %>% slice_sample(n = 1, replace = TRUE)
  
  # Break if it hangs
  i = i + 1
  if (i == 2000) {
    print("Try again")
    i = 1
    new_df <- rs_new_df
    df_copy <- rs_df_copy
  }
  
  # Make temp df of final three rows for check for consecutive
  temp <-
    as.data.frame(rbind(new_df[(nrow(new_df) - 1):(nrow(new_df)),], roi))
  
  temp$big_jar <- 0
  temp$big_jar[temp$j == 1 & temp$reward == "R"] <- "BigJar_R"
  temp$big_jar[temp$j == 1 & temp$reward == "L"] <- "BigJar_L"
  temp$big_jar[temp$j == 2 & temp$reward == "R"] <- "BigJar_L"
  temp$big_jar[temp$j == 2 & temp$reward == "L"] <- "BigJar_R"
  
  if (!any(apply(temp[, 1:6], 2, function(x)
    # Check for consecutive
    has_consecutive(x, 3)))) {
    # If pass, bind new roi
    new_df <- rbind(new_df, roi)
    
    # # If pass, delete from pool of options (this is gross code)
    death <-
      !paste0(rownames(df_copy),
              df_copy[, 1],
              df_copy[, 2],
              df_copy[, 3],
              df_copy[, 4],
              df_copy[, 5]) %in% paste0(rownames(roi), roi[1, 1], roi[1, 2], roi[1, 3], roi[1, 4], roi[1, 5])
    df_copy <- df_copy[death,]
    
  }

}

# Add tick boxes and column names
df_save <-
  cbind(c(1:10, 1:10), new_df, "[  ]", "[  ]", "", "[  ]", "")
colnames(df_save) <-
  c("Trial",
    "Sample",
    "Reward",
    "Move",
    "End",
    "Condition",
    "L",
    "R",
    "",
    "✓",
    "")

# Order columns
df_save <- df_save[,c(1,6,3,2,4,5,7,8,9,10)]

# Write a csv with 20 trials that were randomised
write.csv(
  x = df_save,
  file = paste0("List_20_", i, ".csv"),
  row.names = FALSE
)

# Print message where sheets are saved
cat(paste("Sheets saved in: ",as.character(getwd()),sep=""))
