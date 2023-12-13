library(tidyverse)
library(dplyr, warn.conflicts = FALSE)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Prep

input <- readLines("./Day_13/Part_1/input.txt")

data <-
  data.frame(map = input) %>% 
  mutate(plot_id = 1+cumsum(map == "")) %>%
  group_by(plot_id) %>% 
  filter(map != "") %>% 
  mutate(rowid = row_number()) %>%
  separate_rows(map, sep="", convert = T) %>% 
  drop_na(map) %>% 
  filter(map != "") %>% 
  group_by(plot_id, rowid) %>% 
  mutate(colid = row_number()) %>% 
  ungroup() %>% 
  mutate(key = paste(plot_id, rowid, colid, sep=","))

score <- list()
for(plot in unique(data$plot_id)){
  # plot <-  unique(data$plot_id)[[97]]
  print(paste("Plot", plot,"out of", max(data$plot_id)))
 
  target <- 
    data %>% filter(plot_id ==plot)
  
  col_check <- 1
  row_check <- 1
  max_col = max(target$colid)
  max_row = max(target$rowid)
  result <- FALSE
  result_col <- FALSE
  result_row <- FALSE
  while(!result){
    
    if(col_check<max_col){
      distance_left <- (col_check)
      distance_right <- (max_col-col_check)
      if(distance_left<distance_right){
        skinny_target <- 
          target %>% 
          filter(colid<=(col_check+distance_left)) 
      }else{
        skinny_target <- 
          target %>% 
          filter(colid>(col_check-distance_right))
      }
      
      result_col <- 
        skinny_target %>% 
        group_by(rowid, colid, map) %>% 
        summarise(n = n()) %>% 
        mutate(n = if_else(colid>col_check, -n, n)) %>% 
        mutate(colid = if_else(colid>col_check, (colid-(colid-col_check)*2)+1, colid)) %>% 
        group_by(rowid, colid, map) %>% 
        summarise(match = sum(n)==0) %>% 
        pull(match) %>% 
        all()
      
      if(result_col){
        score[[paste(plot, col_check, row_check)]] <- col_check
        print("Col Found")
      }
      col_check <- col_check+1
    }
    if(row_check<max_row){
      distance_left <- (row_check)
      distance_right <- (max_row-row_check)
      if(distance_left<distance_right){
        skinny_target <- 
          target %>% 
          filter(rowid<=(row_check+distance_left)) 
      }else{
        skinny_target <- 
          target %>% 
          filter(rowid>(row_check-distance_right))
      }
      
      result_row <- 
        skinny_target %>% 
        group_by(colid, rowid, map) %>% 
        summarise(n = n()) %>% 
        mutate(n = if_else(rowid>row_check, -n, n)) %>% 
        mutate(rowid = if_else(rowid>row_check, (rowid-(rowid-row_check)*2)+1, rowid)) %>% 
        group_by(rowid, colid, map) %>% 
        summarise(match = sum(n)==0) %>% 
        pull(match) %>% 
        all()
      
      
      if(result_row){
        score[[paste(plot, col_check, row_check)]] <- (row_check)*100
        print("Row Found")
      }
      row_check <- row_check+1
    }
    result <- (result_col|result_row)
    if((row_check==max_row)&(col_check==max_col)){print("BAD TABLE")}
  }
}
unlist(score) %>% sum()
