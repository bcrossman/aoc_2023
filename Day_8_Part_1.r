library(tidyverse)
library(unglue)
# Prep

file <- readLines("./Day_8/Part_1/input.txt")

input <- 
  unglue_data(file, 
                "{source} = ({L}, {R})",
              convert = TRUE) %>% 
  drop_na(source) %>% 
  column_to_rownames(var = "source")

instructions <- file[[1]] %>% str_split(pattern = "")
instructions <- instructions[[1]]

## Part 1

overall_count <- 1
location <- "AAA"
while(location != "ZZZ"){
  movement <- instructions[[ (overall_count - 1) %% length(instructions) + 1 ]]
  location <- input[location, movement]
  overall_count = overall_count+1
}

location
overall_count-1

input_matrix <- as.matrix(input)

locations <- row.names(input_matrix)[substr(row.names(input_matrix), 3, 3) == "A"]
individual_solves <- list()

for(location in locations){
  print(location)
  overall_count <- 1
  while((substr(location, 3, 3) != "Z")){
  movement <- instructions[[ (overall_count - 1) %% length(instructions) + 1 ]]
  
  location <- input_matrix[location, movement] %>% as.vector()
  
  overall_count <- overall_count + 1
}
  individual_solves[[location]] <- (overall_count-1)
}

library(pracma)
lcm_vector <- function(vec) {
  result <- vec[[1]]
  for (i in 2:length(vec)) {
    new_num <- vec[[i]]
    result <-Lcm(result, new_num)
  }
  return(result)
}

result <- lcm_vector(individual_solves)

result %>% as.character()
