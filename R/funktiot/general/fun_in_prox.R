# Function to check if corresponding values in two vectors are are within a
# tolerance of each other.
# If 'a' is shorter than 'b', 'a' is recycled one step at a time through 'b'.

# Tolerance given as either absolute or relative allowed difference

# No comprehensive input checks!


in_prox <- function(a, b, abs_tol = NULL, rel_tol = NULL, na_to_f = F) {
  
  # Check that only one type of tolerance input is given
  if (!xor(is.null(abs_tol), is.null(rel_tol)))
    stop("Define either absolute or relative tolerance")
  
  # Calculate tolerance based on the input type
  if (!is.null(rel_tol))
    tol <- rel_tol * pmax(a, b)
  if (!is.null(abs_tol))
    tol <- abs_tol
  
  # Check if values within tolerance
  if (length(a) == length(b)) {
    ans <- abs(a - b) <= tol
    
  } else if (length(b) == 1) {
    ans <- sapply(a, function(.a) {.tol <- tol ; abs(.a - b) <= .tol})
  } else if (length(a) != length(b)) { # Recycle a through b if not equal lengths
    warning(paste(
      deparse(a),"and",deparse(b),"not equal lengths!\n",deparse(a),"recycled.\n"))
    ans <- sapply(a, function(.a) {.tol <- tol ; abs(.a - b) <= .tol})
  }
  
  
  # Option to convert all NAs into FALSE (not being within tolerance)
  if (isTRUE(na_to_f))
    ans[is.na(ans)] <- FALSE
  
  return(ans)
  
}
