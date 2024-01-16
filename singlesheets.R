# Generate single trial sheets
# Written by Millie Johnston
# Version 0.1
# Last updated 2023-01-016

### Randomise hand movements
##-----------------------------------------------
# Trials   20
# Sample   R or L (no more than 2 in a row and equal distribution)
# Reward   R or L (no more than 2 in a row and equal distribution)
# Movement P or C (no more than 2 in a row and equal distribution)
# Final    R or L (no more than 2 in a row and equal distribution)
# Generate 3 sets, then add labels 1,2,3, and then check for distribution again (no more than 2 in a row and equal distribution)
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
comb <- comb[rep(seq_len(nrow(comb)), 5),]

# Make a list for sample hand
sam <- list(sample = c("R", "L"))

# Make a grid of possible sample hand combinations
sam <- expand.grid(sam)

# Make into a data frame for easy replication and binding
sam <- cbind(sam)

# Replicate sample hand 10 times so it now equals 20
sam <- sam[rep(seq_len(nrow(sam)), 10),]

# Function to check if conditions are represented n or more times in a row, returns true of false
has_consecutive <- function(vec, n) {
  any(sapply(rle(as.character(vec))$lengths, function(x)
    x >= n))
}

# Prepare for loop
j = 1 # Start of counter so you can stop it after generating "sheets" lists
df_comp <- data.frame() # Empty data frame to bind the lists
sheets <- 3 # How many sheets you are trying to generate

# Loop to find data frames that pass criteria
for (i in 1:1000000) {
  # Make a random data frame of 20 trials sampled from list, sample without replacement
  df <- comb %>% slice_sample(n = 20, replace = FALSE)
  
  # Check if sample, reward, or movement is represented equals 10, retures true of false
  if (any(apply(t(df), 1, function(x)
    any(!table(x) == 10)))) {
    next
  }
  
  # Check is any value in any column is consecutive more than > or = 3 times, returns true of false
  if (any(apply(df[, 1:3], 2, function(x)
    has_consecutive(x, 3)))) {
    next
  }
  
  # Nested loop to add sample hand, runs check to see if there are any combinations or 3 or more
  for (z in 1:100000) {
    df_s <- as.data.frame(sam) %>% slice_sample(n = 20, replace = FALSE)
    df_s <- cbind(df_s, df)
    if (any(apply(df_s[, 1:2], 2, function(x)
      has_consecutive(x, 3)))) {
      next
    }
    break
  }
  
  # Bind to df and add column names
  df <- cbind(df_s[, 1], df)
  colnames(df) <- c("sample", "reward", "move", "finpos")
  
  # Add condition number to df
  df_temp <- cbind(df, j)
  df_comp <- rbind(df_comp, df_temp)
  
  # Report something was found
  print(paste("Found ", j, " (Iteration ", i, ")", sep = ""))
  
  # Add tick boxes and column names
  df_save <- cbind(1:20, df_temp, "[  ]", "[  ]", "", "[  ]", "")
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
    file = paste0("Iteration_", i, ".csv"),
    row.names = FALSE
  )
  
  # Add 1 to counter
  j = j + 1
  
  # Break once counter is greater than 3
  if (j > sheets) {
    break
  }
  
}
