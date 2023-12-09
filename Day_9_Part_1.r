library(tidyverse)
library(unglue)
# Prep

file <- readLines("./Day_9/Part_1/input.txt")

readings <- (lapply(file, function(x) parse_number(unlist(strsplit(x, " ")))))

results <- list()
count <- 1
for(i in readings){
  # print(i)
  # i <- readings[[2]]
  x_values <- 1:length(i)
  degree <- length(x_values)-1
  fit <- lm(i ~ poly(x_values, degree))
  next_x <- max(x_values) + 1
  predicted_value <- predict(fit, newdata = data.frame(x_values = next_x))[[1]]
  results[[as.character(count)]] <- predicted_value
  count <- count+1
  
}
results %>% unlist() %>% sum()

## Part 2 (only change next_x to 0 instead of next)

results <- list()
count <- 1
for(i in readings){
  # print(i)
  # i <- readings[[2]]
  x_values <- 1:length(i)
  degree <- length(x_values)-1
  fit <- lm(i ~ poly(x_values, degree))
  next_x <- 0  ## Only Change
  predicted_value <- predict(fit, newdata = data.frame(x_values = next_x))[[1]]
  results[[as.character(count)]] <- predicted_value
  count <- count+1
  
}
results %>% unlist() %>% sum() %>% round()
