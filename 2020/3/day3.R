data <- scan("input.txt", what = character())

trees <- function(down, right, vec) {
  downs <- seq(1, length(vec), by = down)
  rights <- (seq_along(downs) * right - right) %% nchar(vec[1]) + 1
  path <- mapply(function(d,r) substr(vec[d], r, r), downs, rights)
  length(which(path == "#"))
}

###a Part 1
trees(1, 3, data)
# [1] 218

### Part 2
hits <- mapply(trees, c(1,1,1,1,2), c(1,3,5,7,1), MoreArgs = list(vec = data))

Reduce(`*`, hits, init = 1)
# 3847183340