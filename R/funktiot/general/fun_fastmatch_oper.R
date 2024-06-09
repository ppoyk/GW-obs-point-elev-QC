# Defines a faster %in% operator
# Should be faster than standard %in%, but which() is maybe faster

`%fin%` <- function(x, table) {
  stopifnot(requireNamespace("fastmatch"))
  fastmatch::fmatch(x, table, nomatch = 0L) > 0L
}
