library(tidyverse)

# Read the file and preprocess
file <- readLines("./Day_12/Part_1/sample.txt")
input <- data.frame(value = file)
input <- separate(input, value, into = c("reading", "result"), sep = " ")


library(memoise)

generate_combinations <- function(s) {
  if (nchar(s) == 0) {
    return("")
  } else {
    first_char <- substr(s, 1, 1)
    rest <- substr(s, 2, nchar(s))
    if (first_char == "?") {
      return(c(paste0(".", generate_combinations(rest)),
               paste0("#", generate_combinations(rest))))
    } else {
      return(paste0(first_char, generate_combinations(rest)))
    }
  }
}

generate_combinations <- memoise(generate_combinations)

pattern_count <- integer(nrow(input))

for (i in seq_along(input$reading)) {
  print(i)
  combinations <- generate_combinations(paste(rep(input$reading[i], 3), collapse = "?"))
  pattern_needed <- paste(rep(input$result[i], 3), collapse = ",")
  
  row_result <- sum(sapply(strsplit(combinations, ""), function(x) {
    x <- x[x != ""]
    grp <- cumsum(c(TRUE, diff(x != ".") != 0))
    pattern_result <- paste(table(grp[x == "#"]), collapse = ",")
    pattern_result == pattern_needed
  }))
  
  pattern_count[i] <- row_result
}

pattern_count
sum(pattern_count)