valid_passwords <- function(filepath, method) {
  vals <- read.delim(filepath, header = FALSE, sep = " ")
  
  relevant_symbols <- gsub(":", "", vals[[2]], fixed = TRUE)
  symbol_counts <- strsplit(vals[[1]], "-")
  password_symbols <- strsplit(vals[[3]], "")
  
  valid_passwords <- mapply(
    method,
    relevant_symbols,
    symbol_counts,
    password_symbols
  )
  
  length(which(valid_passwords))
}

### Part 1
password_complies_1 <- function(symbol, limits, password_elements) {
  match_count <- length(password_elements[password_elements == symbol])
  match_count >= as.integer(limits[1]) && match_count <= as.integer(limits[2])
}

valid_passwords("input.txt", password_complies_1)
# [1] 572

### Part 2
password_complies_2 <- function(symbol, limits, password_elements) {
  xor(
    password_elements[as.integer(limits[1])] == symbol,
    password_elements[as.integer(limits[2])] == symbol
  )
}

valid_passwords("input.txt", password_complies_2)
# [1] 306
