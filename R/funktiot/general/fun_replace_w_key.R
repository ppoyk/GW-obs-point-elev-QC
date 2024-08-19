# Function to replace values within supplied data using identifiers
# Replaces a column of the identifier with the value from another

replace_w_key <- function(data, Kname,
                          toK, # Which key to write vars to
                          vars, # Which variables to select from data
                          fromK, # Which key to retrieve vars from
                          data_from = NULL, vars_from = NULL
                          ) {

  # Input validity checks
  stopifnot(is.character(Kname), is.character(vars),
            vars %in% names(data),
            Kname %in% names(data),
            c(toK,fromK) %in% data[[Kname]])
  if (!is.null(vars_from)) stopifnot(is.character(vars_from))
  if (!is.null(vars_from) && !is.null(data_from)) {
    stopifnot(is.character(vars_from), vars_from %in% names(data_from))
  }
  
  # Coerce identifiers to original format ensure functionality
  toK <- as(toK, class(data[[Kname]]))
  fromK <- as(fromK, class(data[[Kname]]))
  
  
  # Default replacement behaviour based on supplied identifying keys
  if (is.null(c(data_from, vars_from))) {
    data[data[[Kname]] == toK, vars] <-
      data[data[[Kname]] == fromK, vars]
    
  # Use optional arg "data_from"
  } else if (!is.null(data_from) && is.null(vars_from)) {
    data[data[[Kname]] == toK, vars] <-
      data_from[data_from[[Kname]] == fromK, vars]
    
  # Use optional arg "vars_from"
  } else if (is.null(data_from) && !is.null(vars_from)) {
    data[data[[Kname]] == toK, vars] <-
      data[data[[Kname]] == fromK, vars_from]
    
  # Use optional args "data_from" and "vars_from"
  } else {
    data[data[[Kname]] == toK, vars] <-
      data_from[data_from[[Kname]] == fromK, vars_from]
  }
  
  
  return(data)
  
}



# Debug / testing
if (F) {
  test <- data.frame(
    "id"= seq(101, 104),
    "a" = c(1:4),
    "b" = letters[1:4],
    "c" = rep(99, 4))
  replace_w_key(test, "id", toK=102, "a", fromK=103)
  replace_w_key(test, "id", toK=102, c("a","b"), fromK=103)
  
  
  # Using optional args
  replace_w_key(test, "id", toK=102, c("a","b"), fromK=103, vars_from=c("a","c"))
  
  test2 <- data.frame("id"= test$id,
                      "a" = paste0("i",1:4),
                      "d" = 10:7,
                      "e" = rep(0,4))
  replace_w_key(test, "id", toK=102, "a", fromK=103, data_from=test2)
  replace_w_key(test, "id", toK=102, c("a","b"), fromK=103, data_from=test2, vars_from=c("d","e"))
}

