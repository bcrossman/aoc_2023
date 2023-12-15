library(tidyverse)

input <- 
  data.frame(code =  readLines("./Day_15/Part_1/input.txt")) %>% 
  separate_rows(code, sep = ",")

hash  <- function(code) {
  # s <- input$code[[1]]
  current_value <- 0
  for (char in strsplit(code, "")[[1]]) {
    current_value <- ((current_value + as.numeric(charToRaw(char))) * 17) %% 256
  }
  return(current_value)
}

## Part 1
input %>% 
  rowwise() %>% 
  mutate(value = hash(code)) %>% 
  pull(value) %>% 
  sum()

## Part 2

instructions <- 
  input %>% 
  separate(code, into = c("code", "focal_length"), sep = "=|-", ) %>% 
  mutate(focal_length = as.numeric(focal_length)) %>% 
  rowwise() %>% 
  mutate(box = hash(code)) %>% 
  ungroup %>% 
  select(label = code, box, focal_length)

lens_box <- tibble(
  box_number = 0:255,
  lenses = lapply(1:256, function(x) list())
)

update_box <- function(box_num, label, focal_length, lens_box) {
  
  return(lens_box)
}

# Loop 
for (i in 1:nrow(instructions)) {
  # i <- 1
  box_num <- instructions$box[i]
  label <- instructions$label[i]
  focal_length <- instructions$focal_length[i]
  
  if (is.na(focal_length)) {
    lenses_in_box <- lens_box$lenses[[box_num + 1]]
    if(length(lenses_in_box)!=0){
    lens_box$lenses[[box_num + 1]] <- lenses_in_box[sapply(lenses_in_box, function(x) x$label != label)]
    }
  } else {
    lenses_in_box <- lens_box$lenses[[box_num + 1]] 
    
    if (length(lenses_in_box) > 0) {
      existing_lens_index <- which(sapply(lenses_in_box, function(x) x$label == label))
      
      if (length(existing_lens_index) > 0) {
        lenses_in_box[[existing_lens_index]]$focal_length <- focal_length
      } else {
        lenses_in_box <- append(lenses_in_box, list(list(label = label, focal_length = focal_length)))
      }
    } else {
      lenses_in_box <- list(list(label = label, focal_length = focal_length))
    }
    
    lens_box$lenses[[box_num + 1]] <- lenses_in_box
  }
}

# Result
total_focusing_power <- 0
for (box_num in 0:255) {
  lenses_in_box <- lens_box$lenses[[box_num + 1]]
  for (slot_num in seq_along(lenses_in_box)) {
    focal_length <- lenses_in_box[[slot_num]]$focal_length
    lens_focusing_power <- (1 + box_num) * slot_num * focal_length
    total_focusing_power <- total_focusing_power + lens_focusing_power
  }
}

total_focusing_power
