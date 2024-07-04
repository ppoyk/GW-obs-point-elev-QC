# A function to test if columns/subvariables equal a given value
# Output logi results or the value from another col from the passing variable

eq_tester <- function(a, b,
                      test_var, should_eq,
                      prefer = c(NA,"a","b"),
                      out_var = NULL) {
  
  prefer <- match.arg(prefer) # Match args to the three listed above

  # Input argument names
  arg_names <- rlang::fn_fmls_names()
  
  # Collate input objects (a & b) as a named list
  inp_list <- mget(arg_names[length(arg_names) - 4 : length(arg_names)])

  # Test input validity
  stopifnot(do.call(is.character, list(x = c(test_var, prefer))))
  # Check that test_var is found within input objects
  stopifnot(do.call(`%in%`,
                    list(x = rep(test_var, length(inp_list)),
                         table = do.call(cbind,lapply(inp_list, names)))))
  if (!is.null(out_var)) {
    stopifnot(is.character(out_var))
    stopifnot(do.call(`%in%`,
                      list(x = rep(out_var, length(inp_list)),
                           table = do.call(cbind,lapply(inp_list, names)))))
  } 
  
  
  # Evaluate both a & b if they meet the condition of x$test_var = should_eq
  t_results <-
    c(isTRUE(all.equal(a[[test_var]], should_eq)),
      isTRUE(all.equal(b[[test_var]], should_eq)))
  
  # Output the logical vector of meeting the test
  if (is.null(out_var)) {
    return(t_results)
    # If an output colname/variable is defined, output its value
  } else {
    # Get the results of which var meets the equality test
    if (all(t_results == c(T,T))) t_results <- "both"
    if (all(t_results == c(T,F))) t_results <- "a"
    if (all(t_results == c(F,T))) t_results <- "b"
    if (all(t_results == c(F,F))) t_results <- "neither"
    
    # Output the out_var on which of a or b fulfills the test
    # (if both meet, output the preferred variable)
    switch(t_results,
           "both" = {
             if (isTRUE(all.equal(a[[out_var]], b[[out_var]]))) {
               return(a[[out_var]])
               } else {
                 switch(eval(prefer),
                        "a" = return(a[[out_var]]),
                        "b" = return(b[[out_var]]),
                        # Error if no preference was set
                        stop(paste("Both inputs pass but outputs differ!:",
                                   a[[out_var]],"<->", b[[out_var]])))
                 }
             },
           # Output the out_var according to which var matched
           "a" = return(a[[out_var]]),
           "b" = return(b[[out_var]]),
           "neither" = return(NA)
    )
  }
}

# Debug / test
if (F) {
  tes_a <- c("a"=1, "b"=2,"c"=3,"d"=4)
  tes_b <- tes_a +1; tes_b[1] <- 1
  eq_tester(tes_a, tes_b, test_var="a",should_eq=1, out_var="b")
}

