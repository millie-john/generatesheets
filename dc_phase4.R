# Phase 4
# Version 0.1
# Last updated 2024-08-21

### Double choice phase 1

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
data <- list(first_sample = c("R", "L"),
             initial_reward = c("R", "L"),
             first_movement = c("P","C"))

# Make a grid of possible list combinations
comb <- expand.grid(data)

# Add final positions to grid
finpos <- apply(comb[, c(2,3)], 1, function(x)
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

#write.csv(
#  x = df2,
#  file = paste0("dc_phase1_", i, ".csv"),
#  row.names = FALSE
#)

p1 <- df2

###

### Double choice phase 2

rm(data)
  
# Create list of variables
data <- list(first_sample = c("R", "L"),
             initial_reward = c("R", "L"),
             first_movement = c("P","B"))

# Make a grid of possible list combinations
comb <- expand.grid(data)

# Add final positions to grid
finpos <- apply(comb[, c(2,3)], 1, function(x)
  paste0(x[1], x[2]))

finpos <-
  ifelse(finpos == "LP", "L", ifelse(finpos == "RP", "R", ifelse(finpos == "LB", "L", "R")))

comb$first_choice<-finpos

# Double matrix rows
comb<-cbind(comb[rep(seq_len(nrow(comb)), 3), ])

# Add second movement
comb$second_movement<-c(rep("P",8),rep("C",8), rep("B",8))

# Add hand change
comb$hand_change<-c(rep("N",24))

# Add Second choice
second_choice <- apply(comb[, c(4,5)], 1, function(x)
  paste0(x[1], x[2]))

second_choice <-
  ifelse(second_choice == "LP", "L", ifelse(second_choice == "RP", "R", ifelse(second_choice == "LC", "R", ifelse(second_choice == "RC", "L", ifelse(second_choice == "LB", "L", "R")))))

comb$second_choice <- second_choice

# Fix row names
rownames(comb)<-1:24

# Function to check if conditions are represented in a sequence n or more times, in a column, returns true or false. Queries if the sequence "has consecutive", if false, it's a good thing.
has_consecutive <- function(vec, n) {
  any(sapply(rle(as.character(vec))$lengths, function(x)
    x >= n))
}

# Initial reward and second choice
for (i in 1:5000) {
  
  # Make a random data frame of 20 trials sampled from list, sample without replacement
  comb4 <- comb %>% slice_sample(n = 20, replace = FALSE)
  
  if (any(apply(t(comb4[,c(1,ncol(comb4))]), 1, function(x)
    any(!table(x) == 10)))) {
    next
  }
  
  break
  
}

df <- comb4

for (i in 1:500000) {
  
  # Make a random data frame of 10 trials sampled from list, sample without replacement
  df3 <- df %>% slice_sample(n = 20, replace = FALSE)
  
  if (any(apply(df3[,c(1,2,3,4,5,7)], 2, function(x)
    has_consecutive(x, 4)))) {
    next
  }
  
  cat(i)
  
  break
  
}

p2 <- df3

###

### Double choice phase 3

rm(data)

# Create list of variables
data <- list(first_sample = c("R", "L"),
             initial_reward = c("R", "L"),
             first_movement = c("P","B"))

# Make a grid of possible list combinations
comb <- expand.grid(data)

# Add final positions to grid
finpos <- apply(comb[, c(2,3)], 1, function(x)
  paste0(x[1], x[2]))

finpos <-
  ifelse(finpos == "LP", "L", ifelse(finpos == "RP", "R", ifelse(finpos == "LB", "L", "R")))

comb$first_choice<-finpos

# Double matrix rows
comb<-cbind(comb[rep(seq_len(nrow(comb)), 2), ])

# Add second movement
comb$second_movement<-c(rep("C",8), rep("B",8))

# Add hand change
comb$hand_change<-c(rep("Y",16))

# Add Second choice
second_choice <- apply(comb[, c(4,5)], 1, function(x)
  paste0(x[1], x[2]))

second_choice <-
  ifelse(second_choice == "LC", "L", ifelse(second_choice == "RC", "R", ifelse(second_choice == "LB", "R", "L")))

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
  df4 <- df %>% slice_sample(n = 20, replace = FALSE)
  
  if (any(apply(df4[,c(1,2,3,4,5,7)], 2, function(x)
    has_consecutive(x, 4)))) {
    next
  }
  
  cat(i)
  
  break
  
}

p3 <- df4

### Add tags

p1$phase<-"P1"
p2$phase<-"P2"
p3$phase<-"P3"

p4<-rbind(p1,p2,p3)

# This wont work yet!!!
                
for (i in 1:500000) {
  
  # Make a random data frame of 10 trials sampled from list, sample without replacement
  p4n <- p4 %>% slice_sample(n = 20, replace = FALSE)
  
  # Distribution P1 = 6, P2 = 6, P3 = 9
  if (!sum(p4n$phase=="P1")==6) { next }
  if (!sum(p4n$phase=="P2")==6) { next }
  if (!sum(p4n$phase=="P3")==8) { next }

  if (!sum(p4n$first_sample=="L")==10)   { next }
  if (!sum(p4n$initial_reward=="L")==10) { next }
  if (!sum(p4n$first_movement=="C")==3)  { next }
  if (!sum(p4n$first_movement=="P")==10) { next }
  if (!sum(p4n$first_movement=="B")==7)  { next }
  if (!sum(p4n$first_choice=="L")==10)   { next }
  if (!sum(p4n$second_movement=="B")==6) { next }
  if (!sum(p4n$second_movement=="C")==9) { next }
  if (!sum(p4n$second_movement=="P")==5) { next }
  if (!sum(p4n$second_choice=="L")==10)  { next }

  cat(i)
  
  if (any(apply(p4n[,c(1,2,3,4,5,7,8)], 2, function(x)
    has_consecutive(x, 4)))) {
    next
  }
  
  cat(i)
  
  break
  
}
