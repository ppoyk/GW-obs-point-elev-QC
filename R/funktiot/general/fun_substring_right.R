.substring_right <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}
