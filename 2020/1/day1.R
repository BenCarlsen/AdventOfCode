items <- scan(filepath, what = integer())

### Part 1
solver <- function(items) {
  do.call(`*`, as.list(items[(2020L - items) %in% items]))
}

solver(items)
# [1] 996075

### Part 2
solver2 <- function(items) {
  possibilities <- combn(items, 3)
  winner <- possibilities[,colSums(possibilities) == 2020]
  Reduce(`*`, winner, init = 1)
}

solver2(items)
# [1] 51810360
