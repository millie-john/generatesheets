##-----------------------------------------------
WIP
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
data <- list(A_L = c("O", "B", "X"),
             A_R = c("O", "B", "X"),
             M_L = c("O", "B", "X"),
             M_R = c("O", "B", "X"))

# Expand all options
comb <- expand.grid(data)

# Select only matching criteria
comb<-comb[(apply(comb,1,function(x) length(which(x=="X"))==1 & length(which(x=="B"))==1 )),]

# Consecutive test
has_consecutive <- function(vec, n) {
  any(sapply(rle(as.character(vec))$lengths, function(x)
    x >= n))
}

# Make a section of the 12
for(i in 1:1000000) {
  eight <- comb %>% slice_sample(n = 8, replace = FALSE)
  if(all(apply(eight,2,function(x) length(which(x=="B"))>1)) & all(apply(eight,2,function(x) length(which(x=="X"))>1) & all(apply(eight,2,function(x) length(which(x=="O"))>3)))) {
    break
  }
}

# Name R with top line is R
comb$remove <- "L"
eight$remove <- "R"


# Bind 12 plus 8
twenty <- rbind(comb,eight)

for(m in 1:100000000) {
  
  # Mix 20 
  twenty_mixed <- twenty %>% slice_sample(n = 20, replace = FALSE)
  twenty_mixed$final <-gsub("X","",paste0(twenty_mixed$A_L,twenty_mixed$A_R,twenty_mixed$M_L,twenty_mixed$M_R))
  
  # Check for 3 in a row
  if (any(apply(twenty_mixed[,1:4,6], 2, function(x)
    has_consecutive(x, 3)))) {
    next
  }
  
  if (has_consecutive(twenty_mixed[,5], 4)) {
    next
  }
  
  print("pass0")
  
  twenty_mixed_tt <- twenty_mixed[1:10,]
  
  if(any(apply(twenty_mixed_tt[,c(1:4)],2,function(x) length(which(x=="B"))<2))) {
    next
  }
  
  print("pass1")
  
  if(any(apply(twenty_mixed_tt[,c(1:4)],2,function(x) length(which(x=="O"))<4))) {
    next
  }
  
  print("pass2")
  
  if(any(apply(twenty_mixed_tt[,c(1:4)],2,function(x) length(which(x=="X"))<2))) {
    next
  }
  
  print("pass3")
  
  twenty_mixed_bt <- twenty_mixed[11:20,]
  
  if(any(apply(twenty_mixed_bt[,c(1:4)],2,function(x) length(which(x=="B"))<2))) {
    next
  }
  
  print("pass4")
  
  if(any(apply(twenty_mixed_bt[,c(1:4)],2,function(x) length(which(x=="O"))<4))) {
    next
  }
  
  print("pass5")
  
  if(any(apply(twenty_mixed_bt[,c(1:4)],2,function(x) length(which(x=="X"))<2))) {
    next
  }
  
  print("pass6")

  break
    
}

write.csv(
  x = twenty_mixed_tt,
  file = paste0("Randomized_", i, "_L1.csv"),
  row.names = FALSE
)

write.csv(
  x = twenty_mixed_bt,
  file = paste0("Randomized_", i, "_L2.csv"),
  row.names = FALSE
)
