# Phase 1
# Version 0.1
# Last updated 2024-07-10

### Double choice phase 1
##-----------------------------------------------
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

# Initial reward
# First sample
# First movement
# First location
# Second movement
# Hand change?
# Second location

# Create list of variables
data <- list(initial_reward = c("R", "L"),
             first_sample = c("P", "C"),
              first_movement = c("P","C"))

# Make a grid of possible list combinations
comb <- expand.grid(data)

# Add final positions to grid
finpos <- apply(comb[, c(1,3)], 1, function(x)
  paste0(x[1], x[2]))

finpos <-
  ifelse(finpos == "LP", "L", ifelse(finpos == "RP", "R", ifelse(finpos == "LC", "R", "L")))

comb$first_choice<-finpos

# Double matrix rows
comb<-cbind(comb[rep(seq_len(nrow(comb)), 2), ])

# Add second movement
comb$second_movement<-c(rep("P",8),rep("C",8))

# Add hand change
comb$hand_change<-c(rep("N",16))

# Add Second choice
second_choice <- apply(comb[, c(4,5)], 1, function(x)
  paste0(x[1], x[2]))

second_choice <-
  ifelse(second_choice == "LP", "L", ifelse(second_choice == "RP", "R", ifelse(second_choice == "LC", "R", "L")))

comb$second_choice <- second_choice

# Fix row names
rownames(comb)<-1:16

# Function to check if conditions are represented in a sequence n or more times, in a column, returns true or false. Queries if the sequence "has consecutive", if false, it's a good thing.
has_consecutive <- function(vec, n) {
  any(sapply(rle(as.character(vec))$lengths, function(x)
    x >= n))
}

# Initial reward and second choice
for (i in 1:5000) {
  
  # Make a random data frame of 10 trials sampled from list, sample without replacement
  comb4 <- comb %>% slice_sample(n = 4, replace = FALSE)
  
  if (any(apply(t(comb4[,c(1,ncol(comb4))]), 1, function(x)
    any(!table(x) == 2)))) {
    next
  }
  
  break
  
}

df <- rbind(comb,comb4)

for (i in 1:500000) {
  
  # Make a random data frame of 10 trials sampled from list, sample without replacement
  df2 <- df %>% slice_sample(n = 20, replace = FALSE)

  if (any(apply(df2[,c(1,2,3,4,5,7)], 2, function(x)
    has_consecutive(x, 4)))) {
    next
  }
  
  break
  
}

write.csv(
  x = df2,
  file = paste0("List_20_", i, ".csv"),
  row.names = FALSE
)
  
  
  
  





