# Define more compact versions of often used functions.

# Definitions mostly belonging to base-package


# paste
p <- function(..., sep = " ", collapse = NULL, recycle0 = FALSE)
  paste(..., sep = sep, collapse = collapse, recycle0 = recycle0)

# paste0
p0 <- function(...,            collapse = NULL, recycle0 = FALSE)
  paste0(...,            collapse = collapse, recycle0 = recycle0)


# as.numeric
as.num <- function(x, ...)
  as.numeric(x = x, ...)
# is.numeric
is.num <- function(x)
  is.numeric(x = x)

# file.path
f.path <- function(..., fsep = .Platform$file.sep) 
  file.path(..., fsep = fsep)


