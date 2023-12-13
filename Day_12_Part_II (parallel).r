library(tidyverse)
library(foreach)
library(doParallel)

# Register parallel backend
no_cores <- detectCores() - 1  # Leave one core free for system processes
registerDoParallel(cores=no_cores)

start_time <- Sys.time()
times <- 5
# Read the file and preprocess
file <- readLines("./Day_12/Part_1/sample.txt")

input <- 
  data.frame(value = file) %>% 
  mutate(value = trimws(value)) %>% 
  separate(value, into = c("reading", "result"), sep = " ")

build_pattern <- function(lengths) {
  # Start with the beginning of the pattern
  pattern <- "^.*"
  
  # Check if lengths is not empty
  if (length(lengths) > 0) {
    # Loop over each element in the vector and build the pattern
    for (len in lengths) {
      pattern <- paste0(pattern, "[#?]{", len, "}.*")
    }
  }
  
  # Append the ending pattern
  pattern <- paste0(pattern, ".*$")
  
  return(pattern)
}

generate_combinations <- function(s, thus_far = "", pattern_needed, regex_pattern) {
  
  # s <- "?#?#?#?#?#?#?#?"
  # pattern_needed <- paste(rep(input$result[3], 1), collapse = ",")
  # thus_far = "#"
  if (nchar(s) == 0) {
    return(thus_far)
  }
  
  chars <- strsplit(thus_far, "")[[1]]
  chars <- chars[chars != ""]
  grp <- cumsum(c(TRUE, diff(chars != ".") != 0))
  count_hash <- table(grp[chars == "#"])
  row_result <- paste(count_hash, collapse = ",")
  
  if (nchar(row_result) > 0) {
    row_vector <- as.numeric(strsplit(row_result, ",")[[1]])
    pattern_vector <- as.numeric(strsplit(pattern_needed, ",")[[1]])
    # print("result")
    # print(row_vector)
    # print("needed")
    # print(pattern_vector)
    if(length(row_vector)>length(pattern_vector)){
      return(thus_far)
    }
    pattern_vector_truncated <- head(pattern_vector, length(row_vector))
    if (!all(row_vector <= pattern_vector_truncated)) {
      return(thus_far)
    }
    if (grepl(pattern = ",", row_result)) {
      pattern_result <- paste0("^", row_result)
      pattern_result <- sub(",\\d+$", "", pattern_result)
      
      if (!grepl(pattern = pattern_result, pattern_needed)) {
        return(thus_far)
      }
    }
    
    if (!grepl(regex_pattern, paste0(thus_far, s))) {
      return(paste0(thus_far, s))
    }
    
  }
  
  first_char <- substr(s, 1, 1)
  rest <- substr(s, 2, nchar(s))
  if (first_char == "?") {
    return(c(generate_combinations(s = rest, thus_far = paste0(thus_far, "."), pattern_needed, regex_pattern),
             generate_combinations(s = rest, thus_far = paste0(thus_far, "#"), pattern_needed, regex_pattern)))
  } else {
    return(generate_combinations(s = rest, thus_far = paste0(thus_far, first_char), pattern_needed, regex_pattern))
  }
}

# library(memoise)
# generate_combinations <- memoise(generate_combinations)

pattern_count <- integer(nrow(input))

# Using foreach for parallel processing
results <- foreach(i = seq_along(input$reading), .combine = 'c') %dopar% {
  # print(paste(i, length(input$reading)))
  pattern_needed <- paste(rep(input$result[i], times), collapse = ",")
  combinations <- generate_combinations(paste(rep(input$reading[i], times), collapse = "?"), 
                                        pattern_needed = pattern_needed,
                                        regex_pattern = build_pattern(as.numeric(strsplit(pattern_needed, ",")[[1]])))
  
  row_result <- sum(sapply(strsplit(combinations, ""), function(x) {
    x <- x[x != ""]
    grp <- cumsum(c(TRUE, diff(x != ".") != 0))
    pattern_result <- paste(table(grp[x == "#"]), collapse = ",")
    pattern_result == pattern_needed
  }))
  return(row_result)
}



pattern_count <- results

end_time <- Sys.time()
execution_time <- end_time - start_time
print(paste("Execution time: ", execution_time))

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}
data.frame(pattern = pattern_count) %>% write.excel()
sum(pattern_count)
